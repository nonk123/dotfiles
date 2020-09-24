[ "$DISPLAY" ] && wmname LG3D

export _JAVA_AWT_WM_NONREPARENTING=1

for dir in $HOME/.{cargo,local}/bin/; do
    [ -d "$dir" ] && export PATH="$dir:$PATH"
done

for file in ~/.bash/.bash_{profile,aliases,misc}; do
    [ -f "$file" ] && . "$file"
done
