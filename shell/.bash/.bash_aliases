alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias tmux='tmux -2'

alias vnc='vncviewer autoselect=0 lowcolorlevel=2 qualitylevel=5 -dotwhennocursor menukey=Delete acceptclipboard sendclipboard shared display=:0'

alias i='ip a s'

alias x='xrdb ~/.Xresources'
alias b='source ~/.profile'

alias cmake='cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON'

alias pipr='pip3 install -r requirements.txt'

alias msg='logger -p user.emerg'

function say {
    espeak "$@" & disown
}
