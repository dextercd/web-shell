#!/bin/sh

set -eo pipefail

effective_build_type="${PTY_BUILD_TYPE:=${BUILD_TYPE:=debug}}"
build_dir="pty_mngr_build_${effective_build_type}"

cmake --build "$build_dir"
cmake --install "$build_dir" --prefix priv/
