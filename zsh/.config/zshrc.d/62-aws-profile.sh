if [[ -f ~/.aws/credentials ]]; then
    local profile
    profile=$(head -n 1 ~/.aws/credentials | grep '\[.*\]')

    if [[ -n $profile ]]; then
        export AWS_PROFILE=${profile:1:-1}
    fi
fi
