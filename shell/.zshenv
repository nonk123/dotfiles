export VISUAL=emacsclient
export EDITOR=vim

export SSH_AGENT_PID=
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh"

export GPG_TTY=$(tty)
gpg-connect-agent updatestartuptty /bye > /dev/null

export SOURCES_DIR="$HOME/Sources"
[[ ! -d "$SOURCES_DIR" ]] && mkdir -p "$SOURCES_DIR"

export DOTFILES_DIR="$SOURCES_DIR/dotfiles"

if [[ ! -d "$DOTFILES_DIR" ]]; then
    echo "`nonk123/dotfiles` must be installed into $SOURCES_DIR" > /dev/stderr
    sleep 5
    exit 1
fi

export WLR_DRM_NO_ATOMIC=1 # for labwc tearing

if which maybe-sccache &> /dev/null; then
    export CMAKE_C_COMPILER_LAUNCHER=maybe-sccache
    export CMAKE_CXX_COMPILER_LAUNCHER=maybe-sccache
    export RUSTC_WRAPPER=maybe-sccache
fi

export CC=/usr/bin/gcc
export CXX=/usr/bin/g++

export CMAKE_EXPORT_COMPILE_COMMANDS=ON

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
