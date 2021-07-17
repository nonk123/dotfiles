. ~/.bash_common

export _JAVA_AWT_WM_NONREPARENTING=1

# Start sway on tty1.
if [ "$(tty)" = "/dev/tty1" ]; then
    exec sway &> /tmp/sway.log
fi
