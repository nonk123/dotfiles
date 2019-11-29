#!/bin/sh

website="`echo "google.com" | ~/Scripts/dmenu_xresources.sh -i -p "Website:"`"
[ "$website" ] && surf "$website"
