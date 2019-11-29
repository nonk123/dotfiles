#!/bin/sh

color() {
    xrdb -query | grep -m1 $1 | cut -f2
}

fn=`color font`
nb=`color background`
nf=`color foreground`
sb=`color color4`
sf=`color background`

dmenu -fn "$fn" -nb "$nb" -nf "$nf" -sb "$sb" -sf "$sf"
