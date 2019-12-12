#!/bin/bash

counter=0
skip=4

# Killing i3blocks kills this script too, for some reason.
trap -- '' SIGRTMIN+1

while read line; do
    # Skip several lines because my CPU struggles to parse JSON 80 times/sec.
    counter=`expr $counter + 1`
    [ $counter != $skip ] && continue || counter=0

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
done < <(i3blocks) | lemonbar -B `xrdb -query | grep -m1 background | cut -f2`
