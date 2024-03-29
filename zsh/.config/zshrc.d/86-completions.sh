#!/usr/bin/env zsh

if command-exists kubectl; then
    source <(kubectl completion zsh)
fi

if command-exists aws; then
    complete -C /usr/local/bin/aws_completer aws
fi
