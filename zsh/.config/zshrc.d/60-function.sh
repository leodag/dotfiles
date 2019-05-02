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
    echo -n "$@" | md5sum | awk '{ printf "%s\n", $1 }'
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
    which "$1" > /dev/null 2>&1
}

commands-exist() {
    while [[ $# != 0 ]] && command-exists "$1"; do
        shift
    done

    return $#
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
        echo "Diffs a file and it's original version"
        echo "Usage: $0 filename"
        return 50
    fi

    local pkg=$(pacman -Qoq "$1")
    if [[ $? != 0 ]]; then
        echo "Error: No package owns $1"
        return 51
    fi
    local ver=$(pacman -Q "$pkg" | cut -d' ' -f2)
    local tarpath=$(realpath "$1")
    tarpath=${tarpath#/}

    tar xOf /var/cache/pacman/pkg/$pkg-$ver-*.pkg.tar.xz "$tarpath" | diff - "$1"
}
