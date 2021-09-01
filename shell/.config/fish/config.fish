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

# A bit of colouring.
set -g fish_color_command white --bold
set -g fish_color_param white
set -g fish_color_quote 33AA00

# vterm stuff.

# screen and tmux stuff is not included because I don't use them.
function vterm_printf
    printf "\e]%s\e\\" "$argv"
end

function vterm_prompt_end
    vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
end

functions --copy fish_prompt vterm_old_fish_prompt

function fish_prompt
    printf "%b" (string join "\n" (vterm_old_fish_prompt))
    vterm_prompt_end
end

function fish_title
    hostname
    echo ":"
    pwd
end

# Start X on tty1 automatically.
if status is-login
    set -g _JAVA_AWT_WM_NONREPARENTING 1

    if [ (tty) = /dev/tty1 ]
        exec startx &>/tmp/startx.log
    end
end
