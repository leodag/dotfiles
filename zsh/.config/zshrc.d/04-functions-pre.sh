#!/usr/bin/env zsh

first-existing() {
    while [[ $# != 0 ]]; do
        if [[ -f $1 ]]; then
            echo "$1"
            return 0
        fi
        shift
    done

    return 1
}

source-first-existing() {
    local file
    file=$(first-existing "$@")

    if [[ -n $file ]]; then
        source "$file"
    else
        return 1
    fi
}
