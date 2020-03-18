#!/bin/sh

xrdb ~/.Xresources

xcompmgr -n &
dunst &

. ~/.profile

exec emacsclient -c
