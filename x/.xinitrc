#!/bin/sh

xrdb ~/.Xresources

xcompmgr -n &
dunst &

xhost +local:

xdotool key Num_Lock

emacsclient -e "(start-exwm)"
exec emacsclient -c
