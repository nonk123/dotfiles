export HISTCONTROL=ignoreboth
export HISTSIZE=
export HISTFILESIZE=

if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

if [ "$TERM" = "eterm-color" ]; then
    export PS1='\w\$ '
else
    export PS1='\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
fi

export EDITOR="e"

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
