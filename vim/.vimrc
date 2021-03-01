" Wafelack <wafelack@protonmail.com>'s .vimrc v0.1.0

set number numberwidth=4  
let mapleader = "," " leader for commands

syntax enable
set termguicolors

" Completion options
set completeopt=menuone,longest

set makeprg=cargo " I'm a rustacean, so cargo ftw

set scrolloff=15

set path+=** " Getting fuzzy
set wildmenu
set noswapfile
set noundofile
set nowritebackup
set nobackup
set noshowmode

let g:currentmode={
      \ 'n'  : 'NORMAL',
      \ 'v'  : 'VISUAL',
      \ 'V'  : 'VISUAL-LINE',
      \ '^V' : 'VISUAL-BLOCK',
      \ 'i'  : 'INSERT',
      \ 'R'  : 'REPLACE',
      \ 'Rv' : 'VISUAL-REPLACE',
      \ 'c'  : 'COMMAND',
      \ 't'  : 'TERMINAL',
      \ }

function! GetBranch()
  if system('git rev-parse --is-inside-work-tree 2> /dev/null') == 'true'
    return substitute(system('git branch --show-current'), '\n', '', 'g')
  else
    return 'none'
  endif
endfunction

set laststatus=2 " Always display a status line
set statusline=%1*\ %{toupper(g:currentmode[mode()])}\ %* " Displaying mode
set statusline+=%2*\ %{tolower(GetBranch())}\ %* " Display actual git branch
set statusline+=\ %t\ (%f) " Displaying filename
set statusline+=\ %m " Display modified flag
set statusline+=%= " Switching to right side
set statusline+=%{tolower(&filetype)}\  
set statusline+=%3*\ %p%%\ ≡\  
set statusline+=\ \ \ %l/%L\ \ \ col\ :\ %c\ %* 

" Status bar colors
hi User1 guifg=White guibg=#6666ff
hi User2 guifg=#cccccc guibg=#4a4c71
hi User3 guifg=White guibg=#6666ff
hi statusline guibg=#666666 guifg=#282a50

" Reloading vim config without restarting it
noremap <leader>sv :source<space>$MYVIMRC<cr>

" Indent all buffer (because the command is quite long --')
nnoremap <leader>i mmgg=G`m

" Tabs indexing
nnoremap & 1gt<cr>
nnoremap é 2gt<cr>
nnoremap " 3gt<cr>
nnoremap ' 4gt<cr>
nnoremap ( 5gt<cr>
nnoremap - 6gt<cr>

" Vim plug
call plug#begin('~/.vim/plugged')

Plug 'ntk148v/vim-horizon'

call plug#end()

" Colorscheme
colorscheme horizon
