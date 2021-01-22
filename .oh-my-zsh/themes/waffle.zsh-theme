setopt PROMPT_SUBST

PROMPT='%F{red}%n%F{reset_color} at %F{yellow}%m %F{reset_color}in %F{green}%~%F{reset_color} on %F{cyan}$(git branch --show-current)%F{reset_color} %F{gray}[%F{yellow}$(git log --format='%h' -n 1 2> /dev/null)%F{gray}]%F{reset_color}  $(date +"%H:%M")'$'\n''>>> '
RPROMPT=''
