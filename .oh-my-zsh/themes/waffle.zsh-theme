setopt PROMPT_SUBST

if [ -z $STY ]
then
   PROMPT='╭─ %F{red}%n%F{reset_color} at %F{yellow}%m %F{reset_color}
-> %F{cyan}$(git branch --show-current) %F{gray}[%F{yellow}$(git log --format='%h' -n 1 2> /dev/null)%F{gray}]%F{reset_color}
╰─ %F{green}%~%F{reset_color} >>> '
 
else

PROMPT='╭─ %F{red}%n%F{reset_color} at %F{yellow}%m %F{reset_color}
-> (screen) %F{cyan}$(git branch --show-current) %F{gray}[%F{yellow}$(git log --format='%h' -n 1 2> /dev/null)%F{gray}]%F{reset_color}
╰─ %F{green}%~%F{reset_color} >>> '
fi

RPROMPT=''
