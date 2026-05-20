#!/usr/bin/env zsh

if command-exists kubectl; then
    source <(kubectl completion zsh)
fi

if command-exists aws; then
    complete -C /usr/local/bin/aws_completer aws
fi

if commands-exist mise usage; then
    source <(mise completion zsh)
else
    echo "Binary usage missing!"
fi

if command-exists rg; then
    # sed is a workaround for ubuntu without https://github.com/BurntSushi/ripgrep/pull/2957
    . <(rg --generate complete-zsh | sed 's/_rg "$@"/compdef _rg rg/')
fi
