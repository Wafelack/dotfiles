# config.fish | Wafelack

# aliases
function py
python3
end

function rl
source ~/.config/fish/config.fish
end

# Path
set PATH /home/wafelack/ $PATH
set PATH $DENO_INSTALL/bin $PATH
set PATH /home/wafelack/.werb_bin $PATH
set PATH /home/wafelack/.lua $PATH
set PATH /home/wafelack/.gem/ruby/2.7.0/bin $PATH


# Prompt
function fish_prompt
  echo '╭─' (set_color red) $USER (set_color normal)'at'(set_color yellow) $hostname(set_color normal)
  echo '->'(set_color cyan) (git branch --show-current) (set_color normal)'['(set_color yellow)(git log --format='%h' -n 1 2> /dev/null)(set_color normal)']'
  echo '╰─'(set_color green) (pwd)(set_color normal) '» '
end

# Variables
export TERM=xterm-256color
