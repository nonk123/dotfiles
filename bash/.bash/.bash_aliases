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
alias b='source ~/.bashrc'

alias v='vim -u NONE'

alias e='emacsclient -t -a "emacs -t"'

alias fuck='eval $(thefuck $(history | tail -n 2 | head -n 1 | cut -d " " -f 4-))'

alias fluidsynth="fluidsynth -a pulseaudio -m alsa_seq -g 1.0 /home/nonk/Misc/Unison.sf2"

alias cmake="cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
