bindkey -e # emacs mode

bind-if-exists() {
    [[ -n "$1" ]] && bindkey "$1" "$2"
}

bind-if-exists "${terminfo[home]}" beginning-of-line
bind-if-exists "${terminfo[kend]}" end-of-line
bind-if-exists "\e[F" end-of-line

bind-if-exists "${terminfo[kUP]}" history-beginning-search-backward
bind-if-exists "${terminfo[kDN]}" history-beginning-search-forward

bind-if-exists "${terminfo[kich1]}" quoted-insert # Ins
bind-if-exists "${terminfo[kdch1]}" delete-char # Del
bind-if-exists "${terminfo[kcbt]}" reverse-menu-complete # Shift+Tab

bind-if-exists "\e[1;5C" emacs-forward-word
bind-if-exists "\e[1;5D" emacs-backward-word

#bind-if-exists "\eOc" emacs-forward-word
#bind-if-exists "\e[5D" backward-word
#bind-if-exists "\eOd" emacs-backward-word
#bind-if-exists "\e\e[C" forward-word
#bind-if-exists "\e\e[D" backward-word
#bind-if-exists "\e0c" forward-word
#bind-if-exists "\e0d" backward-word

