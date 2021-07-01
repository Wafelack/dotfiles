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

- Window Manager   : [DWM](dwm/)
- Status bar       : [Homemade status bar](dwm/status.c)
- Program launcher : dmenu

---

- Terminal Emulator   : [ST](st_config.h)
- Shell               : [bash](bashrc)
- Text Editor         : [vim](vimrc)
