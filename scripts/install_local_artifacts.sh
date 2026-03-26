#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/.." && pwd)"
release_dir="$repo_root/target/release"
install_root="${MIMIUM_INSTALL_DIR:-$HOME/.mimium}"
plugin_dir="$install_root/plugins"
lib_link_path="$install_root/lib"
source_lib_dir="$repo_root/lib"

case "$(uname -s)" in
	Darwin)
		plugin_ext="dylib"
		;;
	Linux)
		plugin_ext="so"
		;;
	*)
		echo "unsupported platform: $(uname -s)" >&2
		exit 1
		;;
esac

binaries=(
	"mimium-cli"
	"mimium-language-server"
	"mimium-language-server-worker"
	"mimium-fmt"
)

plugin_sources=(
	"libmimium_midi.${plugin_ext}:mimium-midi.${plugin_ext}"
	"libmimium_symphonia.${plugin_ext}:mimium-symphonia.${plugin_ext}"
	"libmimium_guitools.${plugin_ext}:mimium-guitools.${plugin_ext}"
)

require_file() {
	local path="$1"

	if [[ ! -f "$path" ]]; then
		echo "required artifact is missing: $path" >&2
		echo "run cargo build --release before installing local artifacts" >&2
		exit 1
	fi
}

mkdir -p "$install_root" "$plugin_dir"

for binary in "${binaries[@]}"; do
	require_file "$release_dir/$binary"
	cp "$release_dir/$binary" "$install_root/$binary"
done

for plugin_spec in "${plugin_sources[@]}"; do
	source_name="${plugin_spec%%:*}"
	install_name="${plugin_spec##*:}"
	require_file "$release_dir/$source_name"
	cp "$release_dir/$source_name" "$plugin_dir/$install_name"
done

if [[ ! -d "$source_lib_dir" ]]; then
	echo "library directory is missing: $source_lib_dir" >&2
	exit 1
fi

if [[ -e "$lib_link_path" && ! -L "$lib_link_path" ]]; then
	echo "$lib_link_path already exists and is not a symbolic link" >&2
	echo "remove it manually or point MIMIUM_INSTALL_DIR at another location" >&2
	exit 1
fi

ln -sfn "$source_lib_dir" "$lib_link_path"

echo "installed local mimium artifacts to $install_root"
