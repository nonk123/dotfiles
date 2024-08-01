#!/usr/bin/env bash

AUTOSTART=(discord telegram-desktop steam)
export WALLPAPER=~/.wallpaper.png
export X11_TERM=alacritty

# TODO TODO TODO...
[[ $NONK ]] && xrandr &>/dev/null

setxkbmap -layout us,ru -option grp:win_space_toggle -option compose:ins
xset r rate 200 30

dunst &
sxhkd &
clipcatd --no-daemon &

xfce4-panel &
redshift -l 55.9:43.0 -m randr &

if [[ -f "${WALLPAPER}" ]]; then
    # TODO: unbreak.
    #feh --bg-fill "${WALLPAPER}"
    echo bruh......
fi

autostart_delay=3
autostart_interval=1
autostart_idx=0

[[ $NONK ]] && {
    for cmd in $AUTOSTART; do
        delay=$((autostart_delay + autostart_interval * autostart_idx++))
        { sleep ${delay}; "$cmd" } &
    done
}

exec xfwm4 --compositor=on
