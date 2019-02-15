if commands-exist fortune cowsay; then
    fortune | cowsay -W $((COLUMNS * 3/5))
else
    echo "Binaries fortune and/or cowsay missing!"
fi
