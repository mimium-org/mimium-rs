/// copied from otopoiesis code.
/// TODO: move this to mimium-lang/utils/fileloader.
use symphonia::core::io::MediaSource;

pub trait FileManager {
    //required to convert into symphonia::MediaSource
    // type File: std::fs::File;
    type Stream: MediaSource;
    type Error: std::error::Error;
    // fn open_file(&self, path: impl ToString)-> Result<Self::File,Self::Error>;
    fn open_file_stream(&self, path: impl ToString) -> Result<Self::Stream, Self::Error>;
    fn read_to_string(&self, path: impl ToString, str: &mut String) -> Result<(), Self::Error>;
    fn save_file<C: AsRef<[u8]>>(&self, path: impl ToString, content: C)
    -> Result<(), Self::Error>;
}

#[cfg(not(target_arch = "wasm32"))]
mod native {
    use std::io::Read;

    use super::*;
    pub struct NativeFileManager {
        // currently has no member
    }
    impl FileManager for NativeFileManager {
        type Stream = std::fs::File;
        type Error = std::io::Error;
        fn open_file_stream(&self, path: impl ToString) -> Result<Self::Stream, Self::Error> {
            let s = path.to_string();
            let p = std::path::Path::new(&s);
            std::fs::File::open(p)
        }
        fn read_to_string(&self, path: impl ToString, str: &mut String) -> Result<(), Self::Error> {
            let mut file = self.open_file_stream(path)?;
            file.read_to_string(str).map(|_| ())
        }
        fn save_file<C: AsRef<[u8]>>(
            &self,
            path: impl ToString,
            content: C,
        ) -> Result<(), Self::Error> {
            let s = path.to_string();
            let p = std::path::Path::new(&s);
            std::fs::write(p, content)
        }
    }
}

#[cfg(target_arch = "wasm32")]
pub mod web {
    use super::*;
    pub struct WebMediaSource {}
    impl std::io::Read for WebMediaSource {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            todo!()
        }
    }
    impl std::io::Seek for WebMediaSource {
        fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
            todo!()
        }
    }
    unsafe impl Send for WebMediaSource {}
    unsafe impl Sync for WebMediaSource {}
    impl MediaSource for WebMediaSource {
        fn is_seekable(&self) -> bool {
            todo!()
        }

        fn byte_len(&self) -> Option<u64> {
            todo!()
        }
    }
    pub struct WebFileManager {}

    pub struct WebFileError {}
    impl std::error::Error for WebFileError {}
    impl std::fmt::Display for WebFileError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "file operation is currenrly not compatible with wasm")
        }
    }

    impl std::fmt::Debug for WebFileError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("WebFileError").finish()
        }
    }

    impl FileManager for WebFileManager {
        type Stream = WebMediaSource;
        type Error = WebFileError;
        fn open_file_stream(&self, path: impl ToString) -> Result<Self::Stream, Self::Error> {
            Err(WebFileError {})
        }
        fn read_to_string(&self, path: impl ToString, str: &mut String) -> Result<(), Self::Error> {
            Err(WebFileError {})
        }
        fn save_file<C: AsRef<[u8]>>(
            &self,
            path: impl ToString,
            content: C,
        ) -> Result<(), Self::Error> {
            Err(WebFileError {})
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]

static GLOBAL_FILE_MANAGER: native::NativeFileManager = native::NativeFileManager {};
#[cfg(target_arch = "wasm32")]

pub static GLOBAL_FILE_MANAGER: web::WebFileManager = web::WebFileManager {};

pub fn get_global_file_manager() -> &'static impl FileManager {
    &GLOBAL_FILE_MANAGER
}
