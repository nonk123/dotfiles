#!/bin/sh

name=`mpc listall | ~/Scripts/dmenu_xresources.sh -l 20 -i -p "Play track:"`
[ "$name" ] && mpc play && mpc searchplay filename "$name"
