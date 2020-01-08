#!/bin/sh

website="`echo "google.com" | ~/Scripts/dmenu-xresources.sh -i -p "Website:"`"
[ "$website" ] && surf "$website"
