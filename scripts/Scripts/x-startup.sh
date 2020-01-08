#!/bin/sh

xset r rate 250 60
setxkbmap -option 'grp:shift_caps_toggle' -layout us,de,ru &

xrdb ~/.Xresources

# Solid color background.
convert -size 100x100 xc:`xrdb -query | grep -m1 background | cut -f2` /tmp/background.png
feh --bg-fill /tmp/background.png

pkill i3blocks-to-lem
~/Scripts/i3blocks-to-lemonbar.sh &
