# modified commands
alias diff='colordiff'              # requires colordiff package
alias grep='grep --color=auto'
alias more='less'
alias df='df -h'
alias du='du -c -h'
alias mkdir='mkdir -p -v'
alias cnano='nano -S -i -w'
alias ..='cd ../'
alias cd..='cd ../'
alias sudo='sudo '
alias vim='nvim'
#alias vim='emacsclient -c -nw'
alias emacsd='emacs --daemon'
alias emacsc='emacsclient -c'

alias stoppedjobs='jobs -p | wc -l'

# new commands
alias da='date "+%A, %B %d, %Y [%T]"'
alias du1='du --max-depth=1'
alias hist='history | grep $1'      # requires an argument
alias psg='ps -Af | grep $1'         # requires an argument (note: /usr/bin/pg is installed by util-linux)

# ls
alias ls='ls -hF --color=auto'
#alias ls='exa -x'
alias lr='ls -R'
alias ll='ls -l'
#alias ll='exa --git -l'
alias la='ll -A'

# safety features
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm'                    # 'rm -i' prompts for every file
alias ln='ln -i'
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

alias usedmem="free -m | grep Mem: | awk  '{print \$3}'"
alias totmem="free -m | grep Mem: | awk '{print \$2}'"
alias usedswap="free -m | grep Swap: | awk '{print \$3}'"
alias totswap="free -m | grep Swap: | awk '{print \$2}'"

#alias docker='sudo docker'
#alias docker-compose='sudo docker-compose'

