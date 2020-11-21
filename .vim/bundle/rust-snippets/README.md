# rust-snippets

Rusty snippets for Vim

You can edit your snippets as well (they are located in `~/.vim/bundle/rust-snippets/snippets`) or even change your snippets folder by changing the path variable in your ~/.vimrc (`let g:snips_path = "$PATH2SNIPPETS"`)

You can also change the leader for this plugin if `&` is already used (`let g:rusty_map = "$NEW_LEADER"` in your .vimrc)

## Installation

### Vlugger

Run `vlugger install wafelack/rust-snippets` in terminal.

### Pathogen

Run `cd ~/.vim/bundle/ && git clone https://github.com/wafelack/rust-snippets.git` in terminal.

### VimPlug

Place this in your .vimrc:

`Plug 'wafelack/rust-snippets'`

… then run the following in Vim:

```
:source %
:PlugInstall
```

### Vundle

Place this in your .vimrc:

`Plugin 'wafelack/rust-snippets'`

… then run the following in Vim:

```
:source %
:PlugInstall
```
