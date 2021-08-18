alias py="python3"
alias fuckit="git reset --hard HEAD"
alias rl="source $HOME/.bashrc"
alias get_license="curl --silent www.gnu.org/licenses/gpl-3.0.txt -o COPYING"

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

export GUILE_LOAD_PATH="/usr/local/share/guile/site/2.2${GUILE_LOAD_PATH:+:}$GUILE_LOAD_PATH"
export GUILE_LOAD_COMPILED_PATH="/usr/local/lib/guile/2.2/site-ccache${GUILE_LOAD_COMPILED_PATH:+:}$GUILE_COMPILED_LOAD_PATH"

__stat() {
  STATUS=$?
  if [[ $STATUS -ne 0 ]]
  then
    echo -n "$STATUS "
  fi
}
PS1="(\$(git branch 2> /dev/null | grep '^*' | colrm 1 2)) \$(__stat)\$(prompt_pwd.pl) % "
