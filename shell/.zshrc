if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

autoload -Uz compinit promptinit

compinit
promptinit

prompt clint

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

zstyle ':completion:*' menu select
zstyle ':completion:*' rehash true

bindkey -e

autoload -Uz up-line-or-beginning-search down-line-or-beginning-search

zle -N up-line-or-beginning-search
bindkey '^P' up-line-or-beginning-search

zle -N down-line-or-beginning-search
bindkey '^N' down-line-or-beginning-search

bindkey '^[[Z' reverse-menu-complete

function clear-screen-and-scrollback() {
    echoti civis > "$TTY"
    printf '%b' '\e[H\e[2J' > "$TTY"
    zle .reset-prompt
    zle -R
    printf '%b' '\e[3J' > "$TTY"
    echoti cnorm > "$TTY"
}

zle -N clear-screen-and-scrollback
bindkey '^L' clear-screen-and-scrollback

alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'

alias grep='grep --color=auto'

function cfg() {
	source ~/.zshenv
	source ~/.zprofile
	source ~/.zshrc
}
