#!/bin/sh

xrdb ~/.Xresources

xcompmgr -n &
dunst &

# Somehow helps with EXWM not starting up.
xhost +local:

# Apply custom layout.
xkbcomp ~/.xkbmap "$DISPLAY"

# Speed up key repeat.
xset r rate 250 60

# Don't do anything weird on non-matching monitor setups.
# The two screens I have connected are VGA-0 and LVDS (laptop screen).
if xrandr | grep "LVDS connected" && xrandr | grep "VGA-0 connected"; then
    # Panning prevents the right edge of VGA-0 appearing in LVDS.
    xrandr --output VGA-0 --auto --output LVDS --auto --right-of VGA-0 --panning 1366x768+1369+0
fi

# Run Emacs with the NEO init-file and start EXWM.
exec emacs -Q -l ~/.emacs.d/neo-init.el -f exwm-enable
