# aliases
alias rm='rm -i'
alias et='exit'
alias rmold='rm -f ./*~'
alias directory_hog='find $1 -type d | xargs du -sm | sort -g'


alias hpg='history | grep $1'

export EDITOR='emacsclient -n'
export fignore=(.o \~)


# set iterm tab and window names
set_iterm_name() {
  mode=$1; shift
  echo -ne "\033]$mode;$@\007"
}
iterm_both () { set_iterm_name 0 $@; }
iterm_tab () { set_iterm_name 1 $@; }
iterm_window () { set_iterm_name 2 $@; }


function findsrc() {
    find $1 -name \*.hh -print -o -name \*.cc -print
}
