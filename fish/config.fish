# config.fish | Wafelack

# aliases
function py
  python3
end

function please
  sudo
end

function rl
  source $HOME/.config/fish/config.fish
end

# Ssh keys management
eval (ssh-agent -c)
ssh-add

# Gpg   "      "
set GPG_TTY (tty)
export GPG_TTY

# Path
set PATH /home/wafelack/ $PATH
set PATH $DENO_INSTALL/bin $PATH
set PATH /home/wafelack/.werb_bin $PATH
set PATH /home/wafelack/.lua $PATH
set PATH /home/wafelack/.gem/ruby/2.7.0/bin $PATH
set PATH /home/wafelack/.cargo/bin $PATH


# Prompt
function fish_prompt
  echo '╭─'(set_color green) (string replace "/home/wafelack" "~" (pwd))(set_color normal)
  set -l is_git (git rev-parse --is-inside-work-tree 2> /dev/null)
  switch $is_git
    case 'true'
      echo '->'(set_color cyan) (git branch --show-current) (set_color normal)'['(set_color yellow)(git log --format='%h' -n 1 2> /dev/null)(set_color normal)']'
  end
  echo '╰─ » '
end

# Variables
export TERM=xterm-256color
