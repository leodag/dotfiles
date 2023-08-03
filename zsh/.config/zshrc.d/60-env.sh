export PAGER=less
export EDITOR=nvim

add-to-path /usr/lib/ccache/bin

add-to-path "$HOME/.bin"
add-to-path "$HOME/.cargo/bin"

# Do not store less search history
export LESSHISTFILE=-

export DOTNET_CLI_TELEMETRY_OPTOUT=1

if which go > /dev/null 2>&1; then
    export GOROOT=/usr/lib/go
fi
