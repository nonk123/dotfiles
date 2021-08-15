setxkbmap -layout "ch(de),ru" -option grp:win_space_toggle
xset r rate 200 30

dunst &
sxhkd &

lemonbar-status &

exec bspwm
