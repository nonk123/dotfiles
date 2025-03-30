autoload -Uz compinit bashcompinit promptinit

compinit
bashcompinit
promptinit

autoload -Uz vcs_info
prompt redhat

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

export ZSH_CACHE_DIR=$HOME/.cache/zsh
mkdir -p "$ZSH_CACHE_DIR"

zstyle ':completion:*' menu select
zstyle ':completion:*' rehash true
zstyle ':completion:*' verbose yes
zstyle ':completion:*' use-cache on
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' cache-path "$ZSH_CACHE_DIR/.zcompcache"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

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

alias la='/bin/ls --color=auto'
alias ls='la -A'
alias ll='ls -lFh'

alias less='less --use-color'

alias grep='grep --color=auto'

function cfg_file() {
    [[ -f "$1" ]] && source "$1"
}

function cfg() {
    cfg_file ~/.zshenv
    cfg_file ~/.zshrc
}

[[ -s "/home/nonk/.bun/_bun" ]] && source "/home/nonk/.bun/_bun"

if which dnscontrol &> /dev/null; then
    eval "$(dnscontrol shell-completion zsh)"
fi
