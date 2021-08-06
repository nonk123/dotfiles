# Set up keyboard layout.
setxkbmap -layout "ch(de),ru" -option grp:win_space_toggle
xset r rate 200 30

# Cool daemons.
dunst &

# Run EXWM.
exec emacs
