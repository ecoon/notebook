# aliases
alias rm='rm -i'
alias et='exit'
alias rmold='rm -f ./*~'
alias directory_hog='find $1 -type d | xargs du -sm | sort -g'
alias hpg='history | grep $1'

export EDITOR='emacsclient -n'
export fignore=(.o \~)

# completions
setopt magicequalsubst

# custom cd
function cd() {
    new_directory="$*";
    if [ $# -eq 0 ]; then 
        new_directory=${HOME};
    fi;
    builtin cd "${new_directory}" && ls
}
compdef _gnu_generic cd

function findsrc() {
    find $1 -name \*.hh -print -o -name \*.cc -print
}

# set iterm tab and window names
set_iterm_name() {
  mode=$1; shift
  echo -ne "\033]$mode;$@\007"
}
iterm_both () { set_iterm_name 0 $@; }
iterm_tab () { set_iterm_name 1 $@; }
iterm_window () { set_iterm_name 2 $@; }

export CODE_BASE=${HOME}/code

# modulefiles
source /usr/local/Cellar/lmod/8.3.17/init/profile
module use -a ${CODE_BASE}/mpi/modulefiles
module use -a ${CODE_BASE}/seacas/modulefiles
alias moduel="module"

# valgrind usage
alias valgrind='valgrind --dsymutil=yes --track-origins=yes'

# ssh external from ORNL
alias sshout='ssh -o "ProxyCommand=corkscrew snowman.ornl.gov 3128 %h %p"'
alias scpout='scp -o "ProxyCommand=corkscrew snowman.ornl.gov 3128 %h %p"'
alias rsyncout='rsync -e "sshout"'

# ATS
export ATS_BASE=${CODE_BASE}/ats
export PYTHONPATH=${ATS_BASE}/ats_manager:${PYTHONPATH}
module use -a ${ATS_BASE}/modulefiles

# anaconda
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/uec/code/anaconda/2020-07/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/uec/code/anaconda/2020-07/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/uec/code/anaconda/2020-07/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/uec/code/anaconda/2020-07/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
conda activate default



