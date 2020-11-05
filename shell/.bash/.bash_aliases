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
    failsafe /usr/bin/env notify-send "$1" "$2"
}

# Copy stdin to clipboard.
# $@ - misc args for `xclip`.
function clip {
    # Remove the trailing newline from the output.
    sed -z '$ s/\n$//' | xclip -selection clipboard "$@"
}

# Copy a file's contents, enclosing it in a Markdown code-block.
# $1 - the file.
function c {
    local lang=${1##*.} # guess language name from extension
    local b='```'
    # ```$lang on the first line, ``` on the last.
    sed "1s/^/$b$lang\n/;\$s/$/\n$b/" "$1" | clip
}

# A mix between `clip` and `c`: enclose stdin in Mardown code-blocks.
function cclip {
    local b='```'
    sed "1s/^/$b\n/;\$s/$/\n$b/" | clip
}

# Output the contents of clipboard to stdout.
function oclip {
    xclip -selection clipboard -o
}

# Automatically start `ssh-agent` on SSH connection.
# $@ - `ssh` args.
function ssh {
    # SSH_AGENT_PID is unset, or `ssh-agent` isn't running.
    if [[ ! $SSH_AGENT_PID ]] || ! kill -0 $SSH_AGENT_PID; then
        eval $(ssh-agent) > /dev/null
        ssh-add ~/.ssh/id_rsa
    fi

    # Force `ssh-agent` support with -A.
    # It was previously set in the config file.
    /usr/bin/env ssh -A "$@"
}

# `cd` to git repository's root.
# $1 - optional path to a folder within a git repository.
function gcd {
    local dir=${1:-$PWD}
    local root=$(git -C "$dir" rev-parse --show-toplevel)

    if [[ "$root" ]]; then
        cd "$root"
    else
        return 1
    fi
}

# Go up N directories.
# $1 - N, defaults to 1.
function up {
    local N=${1:-1}

    # Cannot go down that easily.
    if ((N <= 0)); then
        return 1
    fi

    local path=""

    for ((i=1; i<=N; i++)); do
        path="$path../"
    done

    cd "$path"
}

# `scp` files to the godserver (tm).
# $@ - the files.
function upload {
    for file in "$@"; do
        scp "$file" nonk@tilde.as205315.net:~/Files/
    done
}

export -f failsafe notify-send clip
