export VISUAL=emacsclient
export EDITOR=(emacsclient -nw)

export SSH_AGENT_PID=
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh"

export GPG_TTY=$(tty)
gpg-connect-agent updatestartuptty /bye > /dev/null

export SOURCES_DIR="$HOME/Sources"
[ ! -d "$SOURCES_DIR" ] && mkdir -p "$SOURCES_DIR"

export DOTFILES_DIR="$SOURCES_DIR/dotfiles"

if [ ! -d "$DOTFILES_DIR" ]; then
    echo "nonk123/dotfiles must be installed into $SOURCES_DIR" > /dev/stderr
    sleep 5
    return 1
fi

export ZSH_SECRETS_FILE="$HOME/.zshsecrets"

if [ -f "$ZSH_SECRETS_FILE" ]; then
    source "$ZSH_SECRETS_FILE"
else
    echo "WARN: failed to find $ZSH_SECRETS_FILE; expect further warnings"
fi

function check-var() {
    if ! env | grep "^$1=" > /dev/null; then
	echo "WARN: secret $1 was left unset" > /dev/stderr
    fi
}

#check-var DISTCC_HOSTS

#export DISTCC_FALLBACK=0

#export CMAKE_C_COMPILER_LAUNCHER="sccache"
#export CMAKE_CXX_COMPILER_LAUNCHER="sccache"

export CC="/usr/bin/gcc"
export CXX="/usr/bin/g++"

export CMAKE_EXPORT_COMPILE_COMMANDS=on

export DOTNET_ROOT=~/.dotnet

typeset -U path PATH

path=( \
      ~/.local/bin \
      ~/.cargo/bin \
      ~/go/bin \
      ~/.yarn/bin \
      $DOTNET_ROOT \
      $DOTNET_ROOT/tools \
      $path \
)

export PATH
