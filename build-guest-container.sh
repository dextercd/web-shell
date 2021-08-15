#!/bin/sh

set -eo pipefail

cmake --build wasm_terminal_build_release/ -t terminfo
docker build -t guest:latest  -f guest.Dockerfile .
