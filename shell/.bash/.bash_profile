export HISTCONTROL=ignoreboth
export HISTSIZE=
export HISTFILESIZE=

if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

function color {
    echo "\e[0;$1m$2\e[m"
}

left="[$(color 34 '\D{%d.%m.%Y}')]"
left="$left [$(color 34 '\D{%I:%M}')]"
left="$left j\\j"
left="$left\n$(color 36 \\w)\\$ "

right="$(color 32 \\u@\\H)"

export PS1="\e7$(printf "%*s" $COLUMNS "$right")\e8$left"

export VISUAL="emacsclient"
export EDITOR="emacsclient"

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
