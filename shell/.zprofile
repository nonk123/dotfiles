# Load external zsh module(s).
local external_dir="$DOTFILES_DIR/external"
source "$external_dir/zsh-renew-tmux-env/zsh-renew-tmux-env.plugin.zsh"

# Start ssh-agent automatically. Stolen from tmux FAQ and adapted.

local ssh_agent_file=~/.ssh.agent

if [ ! -f "$ssh_agent_file" ]; then
	ssh-agent -s > "$ssh_agent_file"
fi

eval $(cat "$ssh_agent_file") > /dev/null

if ! kill -0 "$SSH_AGENT_PID" 2> /dev/null; then
	ssh-agent -s > "$ssh_agent_file"
	eval $(cat "$ssh_agent_file") > /dev/null
fi

if [ -n "$SSH_AUTH_SOCK" ]; then
	ssh-add 2> /dev/null
fi

# Start X on tty1 automatically. Stolen from Arch Wiki and adapted.

if [ "$(tty)" = "/dev/tty1" ]; then
    exec startx
fi
