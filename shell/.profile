[ "$DISPLAY" ] && wmname LG3D
export _JAVA_AWT_WM_NONREPARENTING=1

[ -d "$HOME/.local/bin/" ] && export PATH="$HOME/.local/bin:$PATH"
[ -d "$HOME/.cargo/bin/" ] && export PATH="$HOME/.cargo/bin:$PATH"

. ~/.bashrc
