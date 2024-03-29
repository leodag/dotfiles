if [[ -n $TILIX_ID ]] || [[ -n $VTE_VERSION ]]; then
    source-first-existing /etc/profile.d/vte{,-2.91}.sh

    __vte_osc7
fi
