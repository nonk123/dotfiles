#!/bin/sh

xrdb ~/.Xresources

xcompmgr -n &
dunst &

exec emacsclient -c
