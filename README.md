dotfiles
========

My dotfiles for GNU/Linux (Calculate GNU/Linux), managed with [rdfm](https://github.com/wafelack/rdfm).

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
- Dmenu            : dmenu

---

- Terminal Emulator   : [st](st_config.h)
- Shell               : [bash](bashrc)
- Text Editor         : [vim](vim/)
