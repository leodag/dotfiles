#!/usr/bin/env zsh

if [[ -f /home/leonardo.dagnino/.local/bin/mise ]]; then
    source <(/home/leonardo.dagnino/.local/bin/mise activate zsh)
else
    echo "Mise not installed!"
fi
