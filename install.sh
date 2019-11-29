#!/bin/sh

for dir in `ls -d */`; do
    stow "$dir"
done
