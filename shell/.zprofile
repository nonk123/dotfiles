# Load external zsh module(s).
local external_dir="$DOTFILES_DIR/external"
source "$external_dir/zsh-renew-tmux-env/zsh-renew-tmux-env.plugin.zsh"

# Set up ssh-agent through GNOME Keyring.
export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/keyring/ssh
ssh-add 2> /dev/null

# Start X on tty1 automatically. Stolen from Arch Wiki and adapted.
if [ "$(tty)" = "/dev/tty1" ]; then
    exec startx
fi
