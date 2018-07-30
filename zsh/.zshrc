# If not running interactively, don't do anything
[[ $- != *i* ]] && return

for f in $HOME/.config/zshrc.d/*.sh; do
    source "$f"
done

