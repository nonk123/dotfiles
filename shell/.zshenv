export VISUAL=vscodium
export EDITOR=helix

export SOURCES_DIR="$HOME/Sources"
[ ! -d "$SOURCES_DIR" ] && mkdir -p "$SOURCES_DIR"

export DOTFILES_DIR="$SOURCES_DIR/dotfiles"

if [ ! -d "$DOTFILES_DIR" ]; then
	echo "nonk123/dotfiles must be installed into $SOURCES_DIR" > /dev/stderr
	sleep 5
	return 1
fi

typeset -U path PATH

path=( \
	~/.local/bin \
	~/.cargo/bin \
	~/go/bin \
	~/.yarn/bin \
	$path \
)

export PATH
