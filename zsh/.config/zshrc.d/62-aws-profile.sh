#!/usr/bin/env zsh

if [[ -f ~/.aws/credentials ]]; then
    AWS_PROFILE=$(head -n 1 ~/.aws/credentials | grep '\[.*\]')

    if [[ $AWS_PROFILE =~ '^\[.+\]$' ]]; then
        export AWS_PROFILE=${AWS_PROFILE:1:-1}
    else
        unset AWS_PROFILE
    fi
fi
