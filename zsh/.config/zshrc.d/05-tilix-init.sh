if [[ -n $TILIX_ID ]] || [[ -n $VTE_VERSION ]]; then
    source /etc/profile.d/vte.sh
    __vte_osc7
fi
