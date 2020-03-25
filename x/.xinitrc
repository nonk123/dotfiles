#!/bin/sh

xrdb ~/.Xresources

xcompmgr -n &
dunst &

. ~/.profile

emacsclient -e "(start-exwm)"

exec emacsclient -c
