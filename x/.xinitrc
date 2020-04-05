#!/bin/sh

xrdb ~/.Xresources

xcompmgr -n &
dunst &

xhost +local:

emacsclient -e "(start-exwm)"
exec emacsclient -c
