function () {
    local SSH_COLOR
    local SSH_FAILCOLOR
    local ROOT_COLOR
    local ROOT_FAILCOLOR
    local NORMAL_COLOR
    local NORMAL_FAILCOLOR
    local U_C
    local U_FC
    local UN_C

    TITLEBAR='\033]0;%n@%m %4~\007'

    SSH_COLOR=cyan
    SSH_FAILCOLOR=red

    ROOT_COLOR=red
    ROOT_FAILCOLOR=yellow

    NORMAL_COLOR=green
    NORMAL_FAILCOLOR=red

    if [[ -n "$SSH_CLIENT" ]]; then
        U_C=${SSH_COLOR}
        U_FC=${SSH_FAILCOLOR}

        if [[ $UID == 0 ]]; then
            UN_C=${ROOT_COLOR}
        else
            UN_C=${U_C}
        fi
    else
        if [[ $UID == 0 ]]; then
            U_C=${ROOT_COLOR}
            U_FC=${ROOT_FAILCOLOR}
        else
            U_C=${NORMAL_COLOR}
            U_FC=${NORMAL_FAILCOLOR}
        fi
        UN_C=${U_C}
    fi

    precmd() {
        print -Pn "$TITLEBAR"
        rehash
        #echo -ne "\a"
    }

    PROMPT="\
%F{$U_C}%B[%(1j.%F{$U_FC}.%F{$U_C})%j%F{$U_C}]+%f Returned %(?.%F{$U_C}.%F{$U_FC})%?%f%b
%F{$U_C}%B[%*]%b%F{$U_C}[%F{$UN_C}%n%F{$U_C}@%m %4~]%B%f
%#%b "
}


