
if source-first-existing /usr/share/fzf/key-bindings.zsh /usr/share/doc/fzf/examples/key-bindings.zsh; then
    source-first-existing /usr/share/fzf/completion.zsh /usr/share/doc/fzf/examples/completion.zsh
else
    echo 'fzf not installed!'
fi
