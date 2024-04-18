#!/usr/bin/env zsh

if [[ -d ~/.kubech ]]; then
    source ~/.kubech/kubech
    fpath=("$HOME/.kubech/completion" "${fpath[@]}")
    KUBECH_NAMESPACE_CHECK=label
fi
