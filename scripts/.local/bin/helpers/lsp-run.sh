#!/usr/bin/env bash

# Helper script used in `lsp-remote`.
# Run the language server and the pre-start "hooks".

# $1 - project root on the remote machine.
# $2-n - language server command.

cd "$1"

shift

# Run `pip` silently.
function pip() {
    /usr/bin/env pip "$@" < /dev/null &> /dev/null
}

# Start a new virtual environment if requirements.txt is found.
# This marks the project as a Python project.
if [[ -f "requirements.txt" ]]; then
    # Create if virtual environment doesn't exist.
    [[ ! -d env/ ]] && python3 -m virtualenv env/
    source env/bin/activate
    # Install `pyls`.
    pip install -U python-language-server rope pyflakes yapf
    # And random dependencies on top of that, in the background.
    pip install -U -r requirements.txt &
fi

# Fetch Cargo dependencies for Rust completion.
if [[ -f Cargo.toml ]]; then
    cargo fetch &
fi

# Run the language server.
"$@"

wait
