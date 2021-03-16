" My .vimrc
" By Wafelack<wafelack@protonmail.com>
" Available @ https://github.com/wafelack/dotfiles/

" Line numbers
" ============

set number numberwidth=4 
set relativenumber

" Relative nline numbers only in normal and visual mode (useful for jumping)
augroup numbers
	autocmd!
	au InsertEnter * set norelativenumber
	au InsertLeave * set relativenumber
augroup end

let mapleader = "," " Leader for commands

syntax enable
set termguicolors

" Completion options
set completeopt=menuone,longest

set makeprg=cargo " Using Rustlang all day, so cargo ftw

set scrolloff=15 " Minimum of lines show around the current line

set path+=** " Fuzzy path search
set wildmenu " Menu options

" We have source control for that (Copyright https://github.com/Chewie for the joke).
set noswapfile
set noundofile
set nobackup
set nowritebackup

" Status bar replaces that
set noshowmode

" Maps
" ====

nnoremap <leader>sv :source<space>$MYVIMRC<cr>
nnoremap <leader>i mmgg=G`m

" Tabs
" ====

nnoremap & 1gt<cr>
nnoremap é 2gt<cr>
nnoremap " 3gt<cr>
nnoremap ' 4gt<cr>
nnoremap ( 5gt<cr>
nnoremap - 6gt<cr>

" File explorer
" =============

let g:netrw_baner=0
let g:netrw_browse_split=4
let g:netrw_altv=1
let g:netrw_liststyle=3 "| Tree view

nnoremap <leader>t :Vexplore<cr>

" Plugins
" =======

call plug#begin('~/.vim/plugged')

Plug 'ntk148v/vim-horizon' "| Theme

Plug 'vim-airline/vim-airline' "| Status bar
Plug 'vim-airline/vim-airline-themes' "| Status bar

Plug 'jiangmiao/auto-pairs' "| Bracket / Quotes / whatever pairing
Plug 'kien/rainbow_parentheses.vim' "| Bracket colorization

Plug 'wakatime/vim-wakatime' "| Coding activity (kinda tracking)

Plug 'dense-analysis/ale' "| Linting

Plug 'vimsence/vimsence' "| Rich presence, eh flexing on discord is important.

call plug#end()

" Colorscheme
" ===========
colorscheme horizon

" Status line
" ===========
let g:airline_theme='jellybeans'

" Linter
" ======

set omnifunc=ale#completion#OmniFunc
let g:ale_completion_enabled = 1
let g:ale_completion_autoimport = 1
let g:ale_sign_column_always = 1
let g:ale_fix_on_save = 1
let g:ale_linters = {'rust': ['analyzer']}
