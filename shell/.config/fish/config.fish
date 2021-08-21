#!/usr/bin/env fish

set -g fish_greeting ""

# Epic environment variables.
set -g PATH $HOME/.local/bin:$PATH

set -g VISUAL emacsclient
set -g EDITOR emacsclient

# Pretty cool aliases.

alias ls 'ls --color=auto'
alias ll 'ls -alF'
alias la 'ls -A'

alias grep 'grep --color=auto'

if status is-login
    set -g _JAVA_AWT_WM_NONREPARENTING 1

    # Start X on tty1 automatically.
    if [ (tty) = /dev/tty1 ]
        exec startx &> /tmp/startx.log
    end
end
