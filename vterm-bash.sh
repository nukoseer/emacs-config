#!/bin/bash

# Put this block to ~/.bashrc
# if [[ "$INSIDE_EMACS" = 'vterm' ]] \
#     && [[ -n ${EMACS_VTERM_PATH} ]] \
#     && [[ -f ${EMACS_VTERM_PATH}../../vterm-bash.sh ]]; then
# 	source ${EMACS_VTERM_PATH}../../vterm-bash.sh
# fi

if [ "$INSIDE_EMACS" = 'vterm' ]; then
    clear() {
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi

vterm_printf() {
    if [ -n "$TMUX" ] \
        && { [ "${TERM%%-*}" = "tmux" ] \
            || [ "${TERM%%-*}" = "screen" ]; }; then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
PS1=$PS1'\[$(vterm_prompt_end)\]'


find_file() {
    vterm_cmd find-file "$(realpath "${@:-.}")"
}

say() {
    vterm_cmd message "%s" "$*"
}

