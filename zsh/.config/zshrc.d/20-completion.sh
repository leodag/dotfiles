#!/usr/bin/env zsh

# The following lines were added by compinstall

zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion:*' list-prompt %S%p %l%s
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' menu select=1
zstyle ':completion:*' select-prompt %S%p %l%s
zstyle ':completion:*' verbose true
zstyle :compinstall filename '/home/leodag/.config/zshrc.d/20-completion.sh'

autoload -Uz compinit
# End of lines added by compinstall
zstyle ':completion:*' rehash true

autoload bashcompinit
