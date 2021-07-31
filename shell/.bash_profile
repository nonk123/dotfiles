. ~/.bash_common

export _JAVA_AWT_WM_NONREPARENTING=1

# Start X on tty1 automatically.
if [ "$(tty)" = "/dev/tty1" ]; then
    exec startx &> /tmp/startx.log
fi
