#!/bin/sh

xrdb ~/.Xresources

xcompmgr -n &
dunst &

xhost +local:

xkbcomp ~/.xkbmap $DISPLAY

xset r rate 250 60
xsetroot -cursor_name left_ptr

xdotool key Num_Lock

emacsclient -e "(start-exwm)"
exec emacsclient -c
