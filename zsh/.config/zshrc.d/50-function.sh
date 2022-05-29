up() {
    local n=${1:-1}

    case "${n#[-+]}" in
        *[!0-9]* ) echo Not a number: $1
                   return 2
                   ;;
    esac

    if (( $n < 1 )); then
        return 1
    fi

    for i in {1..$n}; do
        cd ..
    done
}

md5() {
    echo -n "$1" | md5sum | cut -d ' ' -f 1
}

x() {
    if [[ -f "$1" ]]; then
        case "$1" in
            *.lrz)      lrztar -d "$1"
                        ;;
            *.tar.bz2)  tar xjf "$1"
                        ;;
            *.tar.gz)   tar xzf "$1"
                        ;;
            *.tar.xz)   tar Jxf "$1"
                        ;;
            *.bz2)      bunzip2 "$1"
                        ;;
            *.rar)      rar x "$1"
                        ;;
            *.gz)       gunzip "$1"
                        ;;
            *.tar)      tar xf "$1"
                        ;;
            *.tbz2)     tar xjf "$1"
                        ;;
            *.tgz)      tar xzf "$1"
                        ;;
            *.zip)      unzip "$1"
                        ;;
            *.Z)        uncompress "$1"
                        ;;
            *.7z)       7z x "$1"
                        ;;
            *)          echo "don't know how to extract '$1'..."
                        return 1
                        ;;
        esac
    else
        echo "'$1' is not a valid file!"
        return 125
    fi
}

command-exists() {
    type "$1" > /dev/null
}

commands-exist() {
    while [[ $# != 0 ]] && command-exists "$1"; do
        shift
    done

    return $#
}

add-to-path() {
    if [[ $# != 1 ]]; then
        echo "Adds a directory to PATH, if it exists"
        echo "Usage: $0 directory"
        return 55
    fi

    if [[ -d "$1" ]]; then
        PATH=$1:$PATH
    fi
}

export-terminfo() {
    if [[ $# == 1 ]]; then
        for key val in ${(kv)terminfo}; do
            echo "$key -> $val;;"
        done > "$1"
    else
        echo "Exports all terminfo entries to a file. Kinda buggy, but works"
        echo "Usage: $0 filename"
        return 50
    fi
}

# pacdiff is taken by pacman-contrib
pacfilediff() {
    if [[ $# != 1 || ! -f "$1" ]]; then
        echo "Shows modifications done to a file since installation"
        echo "Usage: $0 filename"
        return 50
    fi

    local pkg
    pkg=$(pacman -Qoq "$1")
    if [[ $? != 0 ]]; then
        echo "Error: No package owns $1"
        return 51
    fi
    local ver=$(pacman -Q "$pkg" | cut -d' ' -f2)
    local tarpath=$(realpath "$1")
    tarpath=${tarpath#/}

    # may contain both .zst and .zst.sig
    local filename=$(ls /var/cache/pacman/pkg/$pkg-$ver-*.pkg.tar.* | head -n1)
    if [[ ! -f $filename ]]; then
        echo "Error: $pkg-$ver not found in pacman's cache"
        return 52
    fi

    tar xOf "$filename" "$tarpath" | diff - "$1"
}

swap() {
    local awk='
/^Name/ { name = $2 }
/^Pid/ { pid = $2 }
/^VmSwap/ { gsub(/^ +/, "", $2); swap = $2; print pid "," name "," swap }'

    cat /proc/*/status | awk -F$'\t' "$awk" | sort -t ',' -k 3 -n -r | column --separator ',' --table --table-columns pid,name,'swap   ' --table-right pid,'swap   ' | less
}

# also works in bash
watchf() {
    if [[ $# < 1 ]]; then
        echo "Watches a shell function"
        echo "Usage: $0 [watch arguments...] function-name [function arguments...]"
        echo "All watch arguments must start with a dash, don't put spaces before values"
        return 50
    fi

    local watchargs=()
    while [[ ${1:0:1} == '-' ]]; do
        watchargs+=("$1")
        shift
    done

    local fun=$1
    shift

    local dec=$(declare -f "$fun")

    if [[ $dec != "" ]]; then
        local tmp=$(mktemp)
        echo "$dec"$'\n'"$fun"' "$@"' > "$tmp"
        watch "${watchargs[@]}" "$(realpath /proc/$$/exe)" "$tmp" "$@"
        rm "$tmp"
    else
        echo "Error: first argument not starting with a dash ($fun) is not a function"
        return 51
    fi
}
