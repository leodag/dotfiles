if source-first-existing /opt/asdf-vm/asdf.sh ~/.asdf/asdf.sh; then
    if [[ -d ~/.asdf/completions ]]; then
        fpath=("$HOME/.asdf/completions" "${fpath[@]}")
    fi
else
    echo "ASDF not installed!"
fi

compinit
