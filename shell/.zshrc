autoload -Uz compinit bashcompinit promptinit

compinit
bashcompinit
promptinit

prompt redhat

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

alias la='/bin/ls --color=auto'
alias ls='la -A'
alias ll='ls -lF'

alias less='less --use-color'

alias grep='grep --color=auto'

function cfg_file() {
    [[ -f "$1" ]] && source "$1"
}

function cfg() {
    cfg_file ~/.zshenv
    cfg_file ~/.zshrc
}

syntax_path=~/Sources/zsh-syntax-highlighting
[[ -d "$syntax_path" ]] || git clone "https://github.com/dracula/zsh-syntax-highlighting" "$syntax_path"
[[ -d "$syntax_path" ]] && source "$syntax_path/zsh-syntax-highlighting.sh"

highlighting_path=/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
[[ -f "$highlighting_path" ]] && source "$highlighting_path"
