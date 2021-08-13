#!/bin/sh

set -eo pipefail

effective_build_type="${WTERM_BUILD_TYPE:=${BUILD_TYPE:=debug}}"
build_dir="wasm_terminal_build_${effective_build_type}"

cmake --build "$build_dir" --target wasm-terminal
cmake --install "$build_dir" --prefix frontend_src/src/generated/
