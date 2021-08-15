alias py="python3"
alias fuckit="git reset --hard HEAD"
alias rl="source $HOME/.bashrc"

export GPG_TTY=$(tty)
export TERM=xterm-256color
export EDITOR=vim

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="/sbin/:$PATH"
export PATH="$HOME/scripts/:$PATH"
export PATH="/usr/local/texlive/2021/bin/x86_64-linux:$PATH"

__stat() {
  STATUS=$?
  if [[ $STATUS -ne 0 ]]
  then
    echo -n "$STATUS "
  fi
}
PS1="(\$(git branch 2> /dev/null | grep '^*' | colrm 1 2)) \$(__stat)\$(prompt_pwd.pl) % "
