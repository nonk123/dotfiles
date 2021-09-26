export VISUAL=nvim
export EDITOR=nvim

export _JAVA_AWT_WM_NONREPARENTING=1

typeset -U path PATH

path=( \
	~/.local/bin \
	~/.cargo/bin \
	~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin/ \
	$path \
)

export PATH
