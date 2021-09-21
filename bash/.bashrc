alias py="python3"
alias fuckit="git reset --hard HEAD"
alias rl="source $HOME/.bashrc"
alias get_license="curl --silent www.gnu.org/licenses/gpl-3.0.txt -o COPYING"
alias ls="ls --color=always"

export GPG_TTY=$(tty)
export EDITOR=vim

export CROSS_PREFIX="$HOME/.cross"
export CROSS_TARGET="x86_64-elf"

export PATH="$CROSS_PREFIX/bin:$PATH"
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

start_ssh_agent() {
    if [[ -z ${SSH_AGENT_PID+x} ]]
    then
        eval $(ssh-agent)
    fi
}

start_ssh_agent
PS1="(\$(git branch 2> /dev/null | grep '^*' | colrm 1 2)) \$(__stat)\$(prompt_pwd.pl) % "

PATH="/home/wafelack/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/wafelack/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/wafelack/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/wafelack/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/wafelack/perl5"; export PERL_MM_OPT;
