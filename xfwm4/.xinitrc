#!/usr/bin/env bash

AUTOSTART=(discord telegram-desktop)
export WALLPAPER=~/.wallpaper.png
export X11_TERM=alacritty

#export GTK_IM_MODULE=ibus
#export QT_IM_MODULE=ibus
#export XMODIFIERS=@im=ibus
#ibus-daemon -rxRd

fixkbd

# Custom screen setup.
[[ $NONK ]] && {
    left_w=1920
    left_h=1080

    right_w=1366
    right_h=768

    xrandr -q | grep -i 'vga1 connected' > /dev/null \
        && xrandr --fb ${left_w}x${left_h} \
                  --output VGA1 --pos 0x0 --size ${left_w}x${left_h} \
                  --output LVDS1 --off

    # NOTE: ditched in favor of a single-screen setup.
    #xrandr --fb $((left_w + right_w))x${left_h} \
    #       --output VGA1 --pos 0x0 --size ${left_w}x${left_h} \
    #       --output LVDS1 --pos ${left_w}x$((left_h - right_h)) --size ${right_w}x${right_h}
}

dunst &
sxhkd &
clipcatd --no-daemon &
flameshot &

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

[[ $NONK ]] && \
    for cmd in $AUTOSTART; do \
        delay=$((autostart_delay + autostart_interval * autostart_idx++)) \
        { sleep ${delay}; "$cmd" } & \
    done

exec xfwm4 --compositor=on
