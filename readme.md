dotfiles
========

My dotfiles for GNU/Linux (Arch GNU/Linux), managed with [rdfm](https://github.com/wafelack/rdfm).

Installing
----------

* With rdfm:
```bash
$ git clone https://github.com/wafelack/dotfiles.git
$ mkdir -p ~/.config
$ mv dotfiles/ ~/.config/.dotfiles/
$ rdfm install
```

Software
--------

- Window Manager   : [DWM](dwm/)
- Permissions controller: [PORK](pork_config.rs)
- Terminal Emulator   : [ST](st_config.h)
- Shell               : [bash](bashrc)
- Text Editor         : [vim](vimrc)
