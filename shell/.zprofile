# Start X on tty1 automatically.
if [ "$(tty)" = "/dev/tty1" ]; then
    exec startx &> /tmp/startx.log
fi
