#!/usr/bin/env zsh

# this should be loaded after anything defining zsh widgets since it depends on wrapping all of them

if ! source-first-existing /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh; then
    echo "Syntax highlighting not installed!"
fi
