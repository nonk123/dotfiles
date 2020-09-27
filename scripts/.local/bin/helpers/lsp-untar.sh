#!/usr/bin/env bash

# Helper script used in lsp-remote.
# Unpack the project archive, creating the project directory if needed.

# $1 - project destination.
# stdin - the archive.

mkdir -p "$1"
tar -xzC "$1"
