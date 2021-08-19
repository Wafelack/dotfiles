" Vim color file
" File: ~/.vim/colors/pastry.vim
" Author: Wafelack <wafelack@riseup.net>
" Last Change: 2021 August 18
" License: GNU General Public License version 3.0 or any later version
" Version: 0.1.0

highlight clear
if exists("syntax_on")
    syntax reset
endif

set background=dark
let colors_name = "pastry"
highlight Normal ctermbg=0
highlight Comment ctermfg=9
highlight Constant ctermfg=6
highlight String ctermfg=2
highlight Character ctermfg=10
highlight Number ctermfg=14
highlight Boolean ctermfg=12
highlight Float ctermfg=14
highlight Identifier ctermfg=5
highlight Function ctermfg=5
highlight Statement ctermfg=3
highlight Type ctermfg=13
highlight Todo ctermbg=11
highlight CursorLine ctermbg=12 cterm=bold
