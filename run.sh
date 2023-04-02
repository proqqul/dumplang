#!/usr/bin/env bash

set -euo pipefail

cd compiler
cabal run
xxd out.bin

cd ../runtime
zig build run
