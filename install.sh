#!/usr/bin/env bash

# A dotfiles install script.
#
# It simply stows all the directories in the repository root.

# Args:
# $@ - extra args to pass to stow.

for dir in `ls -d */`; do
    stow "$@" "$dir"
done
