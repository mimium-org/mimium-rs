use rkyv::{Archive, Deserialize, Serialize};
//on attributes, see https://github.com/rkyv/rkyv/blob/main/rkyv/examples/json_like_schema.rs
#[derive(Archive, Deserialize, Serialize, Debug, PartialEq, Clone)]
// `attr`を使って、rkyvが生成するArchived型にトレイトを実装させる
#[rkyv(attr(derive(Debug, PartialEq)))]
#[rkyv(serialize_bounds(
    __S: rkyv::ser::Writer + rkyv::ser::Allocator,
    __S::Error: rkyv::rancor::Source,
))]
#[rkyv(deserialize_bounds(__D::Error: rkyv::rancor::Source))]
#[rkyv(bytecheck(
    bounds(
        __C: rkyv::validation::ArchiveContext,
    )
))]
pub enum StateTree {
    Delay {
        typesize: u64,
        readidx: u64,
        writeidx: u64,
        data: Box<[u64]>,
    },
    Mem {
        data: Box<[u64]>,
    },
    Feed {
        data: Box<[u64]>,
    },
    FnCall(#[rkyv(omit_bounds)] Vec<StateTree>),
}
