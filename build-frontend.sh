#!/bin/sh

set -eo pipefail

effective_build_type="${FE_BUILD_TYPE:-${BUILD_TYPE:-debug}}"

cd frontend_src
npx webpack --config "webpack.$effective_build_type.js"
