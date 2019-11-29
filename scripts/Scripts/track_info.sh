#!/bin/sh

paused=$([ "`mpc status | grep paused`" ] && echo "⏸️" || echo "▶️")
repeat=$([ "`mpc status | grep \"repeat: on\"`" ] && echo "🔁")
name=$(basename "$(mpc current)")

format="$repeat$paused $name"

[ "$name" ] && echo "$format" || echo
