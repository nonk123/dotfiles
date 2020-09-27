#!/usr/bin/env bash

# Helper script used in `lsp-remote`.
# Copy a file to remote machine, creating parent directories if necessary.

# $1 - file destination.
# stdin - file contents.

mkdir -p "${1%/*}"
cat > "$1"
