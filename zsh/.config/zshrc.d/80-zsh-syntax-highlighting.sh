#!/usr/bin/env zsh

if ! source-first-existing /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh; then
    echo "Syntax highlighting not installed!"
fi
