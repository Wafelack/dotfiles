dotfiles
========

My dotfiles for GNU/Linux (Gentoo), managed with [rdfm](https://github.com/wafelack/rdfm).

Installing
----------

* With rdfm:
```bash
$ git clone https://github.com/wafelack/dotfiles.git
$ mkdir -p ~/.config
$ mv dotfiles/ ~/.config/.dotfiles/
$ rdfm install
# You might need to run the last command as root
# because of some files that aren't writeable
# by a non-root user (E.g. make.conf)
```

Software
--------

- Window Manager   : [i3](i3/)
- Status bar       : [i3blocks](i3blocks/)
- Dmenu            : [rofi](rofi/)

---

- Terminal emulator   : [alacritty](alacritty/)
- Terminal multiplexer: Zellij
- Shell               : [fish](fish/)
- Text Editor         : [vim](vim/)
