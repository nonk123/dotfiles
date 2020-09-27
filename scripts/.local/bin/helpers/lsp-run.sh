#!/usr/bin/env bash

# Helper script used in `lsp-remote`.
# Run the language server and the pre-start "hooks".

# $1 - project root on the remote machine.
# $2-n - language server command.

cd "$1"

shift

# Activate the Python virtual environment and install the project dependencies.
function venv_hook() {
    export VIRTUAL_ENV=$PWD/$1
    source $1/bin/activate
    python -m pip install --upgrade -r requirements.txt &
}

# Check for common virtual environment directories.
for venv_dir in env/ venv/; do
    if [[ -d "$venv_dir" ]]; then
        venv_hook "$venv_dir"
        break
    fi
done

# Fetch Cargo dependencies for Rust completion.
if [[ -f Cargo.toml ]]; then
    cargo fetch &
fi

# Run the language server.
$@

wait
