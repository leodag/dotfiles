#!/usr/bin/env zsh

if [[ -f /usr/share/nvm/init-nvm.sh ]]; then
    source /usr/share/nvm/init-nvm.sh

    if [[ ! $(nvm current) == 'system' ]]; then
        echo "NVM not using system as default"
    fi
fi
