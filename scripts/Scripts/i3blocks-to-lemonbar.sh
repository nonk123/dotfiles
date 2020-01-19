#!/bin/bash

# Input lines to skip to save CPU usage.
skip=3

# Killing i3blocks kills this script too, for some reason.
trap -- '' SIGRTMIN+1

while read line; do
    for i in {0..$skip}; do read line; done

    line="${line:1}"

    echo "$line" | jq 'type' > /dev/null
    [ $? != 0 ] && continue

    align="left"
    out=""

    echo "$line" | jq -c '.[]' | {
        while read block; do
            out="$out%{"
            prevAlign=$align
            align=$(echo "$block" | jq -j '.align?');

            # Aligning multiple times breaks everything.
            [ $align != $prevAlign ] && case $align in
                left)
                    out="${out}l"
                    ;;
                right)
                    out="${out}r"
                    ;;
                center)
                    out="${out}c"
                    ;;
                *)
                    out="${out}r"
                    ;;
            esac

            color=$(echo "$block" | jq -j '.color?')
            [ $color = null ] && color=-

            out="${out}F$color}"
            out="$out $(echo "$block" | jq -j '.full_text?')%{F-}"
        done

        echo -e "$out"
    }
done < <(i3blocks) | lemonbar -B `xrdb -query | grep -m1 background | cut -f2` \
                              -f "`xrdb -query | grep -m1 font | cut -f2`"
