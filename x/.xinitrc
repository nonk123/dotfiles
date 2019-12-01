#!/bin/bash

export _JAVA_AWT_WM_NONREPARENTING=1

setxkbmap -option 'grp:shift_caps_toggle' -layout us,de,ru &
xsetroot -solid "#e6e1dc"

xcompmgr -n &
dunst &
sxhkd &

xrdb ~/.Xresources

convert -size 100x100 xc:`xrdb -query | grep -m1 background | cut -f2` /tmp/background.png
feh --bg-fill /tmp/background.png

exec i3
