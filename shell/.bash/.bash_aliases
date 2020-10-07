alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias b='source ~/.profile'

alias cmake='cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON'

alias pipr='pip3 install --upgrade -r requirements.txt'

# Send a message to syslog. Will be displayed to every logged-in user.
# $1 - message contents.
function msg {
    logger -p user.emerg "$1"
}

# Auto-kill a quick program if it freezes the system.
# $@ - command to run.
function failsafe {
    timelimit -q -t 3 -T 5 /usr/bin/env "$@"
}

# Send a notification and print it to stdout.
# $1 - title.
# $2 - contents.
function notify-send {
    echo "$2"
    failsafe notify-send "$1" "$2"
}

# Copy stdin to clipboard.
function clip {
    # Remove the trailing newline from the output.
    sed -z '$ s/\n$//' | xclip -selection clipboard
}

# Copy a file's contents, enclosing it in a Markdown code-block.
# $1 - the file.
function c {
    local lang=${1##*.} # guess language name from extension
    local b='```'
    # ```$lang on the first line, ``` on the last.
    sed "1s/^/$b$lang\n/;\$s/$/\n$b/" "$1" | clip
}

# Automatically start `ssh-agent` on SSH connection.
# $@ - `ssh` args.
function ssh {
    # SSH_AGENT_PID is unset, or `ssh-agent` isn't running.
    if [[ ! $SSH_AGENT_PID ]] || ! kill -0 $SSH_AGENT_PID; then
        eval $(ssh-agent)
    fi

    /usr/bin/env ssh "$@"
}

export -f failsafe notify-send clip
