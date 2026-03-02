#!/bin/sh
# Copy plugin dylibs to current directory for cargo-dist packaging
# Create empty placeholder files for platforms we're not building for

# Determine the target directory
if [ -n "$CARGO_BUILD_TARGET" ]; then
    TARGET_DIR="../../../target/$CARGO_BUILD_TARGET/release"
else
    TARGET_DIR="../../../target/release"
fi

# Copy actual plugins that exist
for plugin in mimium_symphonia mimium_midi mimium_guitools; do
    for pattern in "lib${plugin}.dylib" "lib${plugin}.so" "${plugin}.dll"; do
        if [ -f "$TARGET_DIR/$pattern" ]; then
            cp "$TARGET_DIR/$pattern" .
            echo "Copied $pattern"
        else
            # Create empty file as placeholder so cargo-dist doesn't fail
            touch "$pattern"
        fi
    done
done
