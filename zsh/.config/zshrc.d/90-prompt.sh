#!/usr/bin/env zsh

autoload -Uz add-zsh-hook

set_titlebar() {
    print -Pn '\033]0;%n@%m %4~\007'
}
add-zsh-hook precmd set_titlebar

# makes print_end_time not run at the first prompt
add_print_end_time() {
    add-zsh-hook -d precmd add_print_end_time
    add-zsh-hook precmd print_end_time
}
add-zsh-hook precmd add_print_end_time

print_end_time() {
    print -P "Finished at %F{$TIMECOLOR}%*%f"
}

# some things taken from jonmosco/kube-ps1
prompt_kube() {
    if [[ $PROMPT_KUBE_ENABLED != "true" ]]; then
        return
    fi

    local ctx
    local ns
    local ctxcolor=grey
    local prompt

    if ! ctx=$(kubectl config current-context 2> /dev/null); then
        return
    fi

    ns=$(kubectl config view --minify -o=jsonpath='{..namespace}')

    if [[ $ctx =~ ^arn: ]]; then
        ## removes longest match of */
        ctx=${ctx##*/}
    fi

    if [[ $KUBECONFIG =~ /config.dest.d/ ]]; then
        ctxcolor=green
    fi

    prompt="%{%F{blue}%}\u2388%{%f%} %{%F{$ctxcolor}%}$ctx%{%f%}"

    if [[ -n $ns ]]; then
        prompt+=":%{%F{blue}%}$ns%{%f%}"
    fi

    echo "$prompt"
}

PROMPT_KUBE_ENABLED=true
setopt PROMPT_SUBST
RPROMPT="\$(prompt_kube)"

kubeon() {
    PROMPT_KUBE_ENABLED=true
}
kubeoff() {
    PROMPT_KUBE_ENABLED=false
}

() {
    local SSH_COLOR
    local SSH_FAILCOLOR
    local ROOT_COLOR
    local ROOT_FAILCOLOR
    local NORMAL_COLOR
    local NORMAL_FAILCOLOR
    local U_C
    local U_FC
    local UN_C

    SSH_COLOR=cyan
    SSH_FAILCOLOR=red

    ROOT_COLOR=red
    ROOT_FAILCOLOR=yellow

    NORMAL_COLOR=green
    NORMAL_FAILCOLOR=red

    if [[ -n $SSH_CLIENT ]]; then
        U_C=$SSH_COLOR
        U_FC=$SSH_FAILCOLOR

        if [[ $UID == 0 ]]; then
            UN_C=$ROOT_COLOR
        else
            UN_C=$U_C
        fi
    else
        if [[ $UID == 0 ]]; then
            U_C=$ROOT_COLOR
            U_FC=$ROOT_FAILCOLOR
        else
            U_C=$NORMAL_COLOR
            U_FC=$NORMAL_FAILCOLOR
        fi
        UN_C=$U_C
    fi

    TIMECOLOR=$U_C
    PROMPT="\
%F{$U_C}%B[%(1j.%F{$U_FC}.%F{$U_C})%j%F{$U_C}]+%f Returned %(?.%F{$U_C}.%F{$U_FC})%?%f%b
%F{$U_C}%B[%D{%H:%M:%S}]%b%F{$U_C}[%F{$UN_C}%n%F{$U_C}@%m %4~]%B%f
%#%b "
}
