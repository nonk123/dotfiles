src() {
    [ -f "$@" ] && . "$@"
}

src ~/.bash/.bash_profile
src ~/.bash/.bash_aliases
src ~/.bash/.bash_misc
