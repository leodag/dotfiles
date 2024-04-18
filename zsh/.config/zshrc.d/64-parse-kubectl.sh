#!/usr/bin/env zsh

# weird parsing commands (z/q/Q) mostly learned from pure
zle-reorder-kubectl() {
    local buf
    local elts
    local kubectl_end
    local parts

    buf=("${(z)BUFFER}")

    if [[ ! ${buf[1]} =~ "kubectl" ]]; then
       return
    fi

    elts=${#buf}
    kubectl_end=elts

    for i in $(seq 1 "$elts"); do
        if [[ ${buf[$i]} == '|' ]]; then
            kubectl_end=$(( i - 1 ))
        fi
    done

    parts=("${(@Q)${(z)$(_reorder_kubectl_parseopts "${(@)buf:1:$((kubectl_end - 1))}")}}")
    parts+=("${(@)buf:$kubectl_end:$elts}")

    BUFFER=$parts

    return
}

_reorder_kubectl_parseopts() {
    local ctx=()
    local ns=()
    local out=()
    local rest=()
    local result

    while [[ $# > 0 ]]; do
        case ${(Q)1} in
            --context)
                [[ $# == 1 ]] && break
                ctx=($1 $2)
                shift; shift
                ;;
            -n|--namespace)
                [[ $# == 1 ]] && break
                ns=($1 $2)
                shift; shift
                ;;
            -o|--output)
                [[ $# == 1 ]] && break
                out=($1 $2)
                shift; shift
                ;;
            *)
                rest+=($1)
                shift
                ;;
        esac
    done

    result=(kubectl "${(@)ctx}" "${(@)ns}" "${(@)rest}" "${(@)out}")

    echo "${(@qq)result}"
}

zle -N zle-reorder-kubectl
bindkey '^[k' zle-reorder-kubectl
