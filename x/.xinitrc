#!/bin/sh

export _JAVA_AWT_WM_NONREPARENTING=1

xrdb ~/.Xresources

xcompmgr -n &
dunst &

exec emacsclient -c
