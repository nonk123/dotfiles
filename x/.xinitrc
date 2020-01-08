#!/bin/sh

export _JAVA_AWT_WM_NONREPARENTING=1

xcompmgr -n &
dunst &
sxhkd &

exec i3
