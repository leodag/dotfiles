#!/usr/bin/env zsh

bindkey -e # emacs mode

declare -A keys

# workaround for incomplete terminfo, or combinations not in terminfo
# Tilix
if [[ -n "$TILIX_ID" ]]; then
    # C-<Right>
    keys[kcrit]="\e[1;5C"
    # C-<Left>
    keys[kclft]="\e[1;5D"
    # C-<Backspace>
    keys[kcbs]="^H"
    # C-<Delete>
    keys[kcdch1]="\e[3;5~"
fi

# Make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
    function zle-line-init() {
        echoti smkx
    }
    function zle-line-finish() {
        echoti rmkx
    }
    zle -N zle-line-init
    zle -N zle-line-finish
fi

key() {
    if [[ -n "${keys[$1]}" ]]; then
        echo "${keys[$1]}"
    else
        echo "${terminfo[$1]}"
    fi
}

bind-if-exists() {
    k=$(key $1)
    [[ -n "$k" ]] && bindkey "$k" "$2"

    if [[ -n "$DEBUGKEYS" && -z "$k" ]]; then
        echo "Key $1 not found"
    fi
}

# Home
bind-if-exists khome beginning-of-line
# End
bind-if-exists kend end-of-line

# S-<Up>
bind-if-exists kUP history-beginning-search-backward
# S-<Down>
bind-if-exists kDN history-beginning-search-forward

# Ins
bind-if-exists kich1 quoted-insert
# Del
bind-if-exists kdch1 delete-char
# S-<Tab>
bind-if-exists kcbt reverse-menu-complete

# C-<backspace>
bind-if-exists kcbs backward-kill-word
# C-<Delete>
bind-if-exists kcdch1 kill-word

# C-<Right>
bind-if-exists kcrit emacs-forward-word
# C-<Left>
bind-if-exists kclft emacs-backward-word

# old binds for urxvt
#bind-if-exists "\eOc" emacs-forward-word
#bind-if-exists "\e[5D" backward-word
#bind-if-exists "\eOd" emacs-backward-word
#bind-if-exists "\e\e[C" forward-word
#bind-if-exists "\e\e[D" backward-word
#bind-if-exists "\e0c" forward-word
#bind-if-exists "\e0d" backward-word
