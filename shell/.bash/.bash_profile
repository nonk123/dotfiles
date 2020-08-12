export HISTCONTROL=ignoreboth
export HISTSIZE=
export HISTFILESIZE=

function color {
    echo "\\[\\e[0;$1m\\]$2\\[\\e[m\\]"
}

export PS1="$(color 32 \\u@\\H):$(color 36 \\w)\\$ "

export VISUAL="emacsclient"
export EDITOR="emacsclient"

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
