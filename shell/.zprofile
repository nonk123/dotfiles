export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/keyring/ssh
ssh-add 2> /dev/null

export WLR_DRM_NO_ATOMIC=1
