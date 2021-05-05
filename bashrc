if [[ $- != *i* ]]
then
	return
fi

export GPG_TTY=$(tty)
export QUARK=$HOME/.quark/
export ORION_LIB=/usr/lib/orion

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.quark/:$PATH"

alias rl="source $HOME/.bashrc"
alias py="python3"

complete -cf rad
complete -W '-V --version --list --explain -v --verbose -q --quiet --color --frozen --locked --offline --config -Z -h --help build b check c clean doc new init run r test t bench update search publish install uninstall' cargo

git_branch() {
	IS_GIT=$(git rev-parse --is-inside-work-tree 2> /dev/null)
	case $IS_GIT in
		"true")
			echo -ne "\033[0;34m\033[41m\033[0;30m\033[41m  $(git branch --show-current) "
			;;
	esac
}

directory() {
	DIRECTORY=$(pwd)
	echo -ne "\033[0;30m\033[44m ${DIRECTORY/$HOME/"~"}"
}

prompt() {
	STATUS=$?
	OUTPUT=$(git_branch)
	echo -n $(directory)" "
	echo -n $OUTPUT
	if [[ -z $OUTPUT ]]
	then
		echo -e "\033[0;34m\033[0m"
	else
		echo -e " \033[0;31m\033[0m"
	fi

	if [[ $STATUS -eq 0 ]]
	then
		echo -ne "\033[0;32m❯ \033[0m"
	else
		echo -ne "\033[0;31m❯ \033[0m"
	fi
}

PS1='$(prompt)'
