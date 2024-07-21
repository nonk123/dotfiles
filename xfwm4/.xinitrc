#!/usr/bin/env bash

AUTOSTART=(discord telegram-desktop steam)
export WALLPAPER=~/.wallpaper.png
export X11_TERM=alacritty

setxkbmap -layout us,ru -option grp:win_space_toggle -option compose:ins
xset r rate 200 30

dunst &
sxhkd &

xfce4-panel &
redshift -l 55.9:43.0 -m randr &

# TODO: port `cliphist` watching for X11.
#wl-paste --watch cliphist store &

if [[ -f "${WALLPAPER}" ]]; then
    # TODO: unbreak.
    #feh --bg-fill "${WALLPAPER}"
    echo bruh......
fi

for cmd in $AUTOSTART; do
    sleep 0.5
    "$cmd" &
done

exec xfwm4 --compositor=on
