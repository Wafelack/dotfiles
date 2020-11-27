" My .vimrc
"
" It is a complete mess so good luck to find things in


set spell 
nnoremap <c-k> :set number!<cr>
set number numberwidth=4  
let mapleader = "," " leader for commands
let maplocalleader = "!" " leader for local commands

" Pick first completion option and apply it
imap <tab><tab> <c-n><c-n><cr> 
nnoremap s <NOP>

" Completion options
set completeopt=menuone,longest

set makeprg=cargo
" Source current file (useful while developing plugins)
nnoremap <leader>sop :source %<cr>
set scrolloff=5
set showcmd " Show the actual command
set cursorline " Highlight current line

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
			\ '' : 'VISUAL-BLOCK',
			\ 'i'  : 'INSERT',
			\ 'R'  : 'REPLACE',
			\ 'Rv' : 'VISUAL-REPLACE',
			\ 'c'  : 'COMMAND',
			\ 't'  : 'TERMINAL',
			\ }

augroup git
	autocmd!
	au BufEnter * let g:branch = substitute(system('git branch --show-current'), '\n', '', 'g')
	au InsertEnter * let g:branch = substitute(system('git branch --show-current'), '\n', '', 'g')
	au InsertLeave * let g:branch = substitute(system('git branch --show-current'), '\n', '', 'g')
augroup end

let g:branch = substitute(system('git branch --show-current'), '\n', '', 'g')


set laststatus=2 " Always display a status line
set statusline=%1*\ %{toupper(g:currentmode[mode()])}\ %* " Displaying mode
set statusline+=%2*\ %{tolower(g:branch)}\ %* " Display actual git branch
set statusline+=\ %t\ (%f) " Displaying filename
set statusline+=\ %m " Display modified flag
set statusline+=%= " Switching to right side
set statusline+=%{tolower(&filetype)}\  
set statusline+=%3*\ %p%%\ ≡\  
set statusline+=\ \ \ %l/%L\ \ \ col\ :\ %c\ %* 

" Remapping enter to select the current item in completion
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>" 

" Leaders remapping
nnoremap <leader>ev :tabfind<space>$MYVIMRC<cr>
noremap <leader>sv :source<space>$MYVIMRC<cr>
nnoremap <leader>d dd
nnoremap <leader>c ddO
vnoremap <leader>" <esc>`<i"<esc>`>i"<esc>v
nnoremap <leader>" viw<esc>a"<esc>bi"<esc>lel
nnoremap <leader>' viw<esc>a'<esc>bi'<esc>lel
nnoremap <leader>i mmgg=G`m
nnoremap <leader>f :call ToggleTabSize()<cr>
nnoremap <cr> $i<Right><cr><esc>

" Uppercase word
nnoremap <c-u> wvbu
" Delete current line
nnoremap <c-d> dd
" Select the whole buffer
nmap <c-a> ggvG$
vmap <c-a> <esc>ggvG$
imap <c-a> <esc>ggvG$
" Increase or decrease panel size
nnoremap is <c-w>>
nnoremap ds <c-w><

function! ToggleTabSize()
	if &tabstop == 4
		set tabstop=2
	elseif &tabstop == 8
		set tabstop=4
	else
		set tabstop=8
	endif
endfunction

" Tabs
nnoremap & 1gt<cr>
nnoremap é 2gt<cr>
nnoremap " 3gt<cr>
nnoremap ' 4gt<cr>
nnoremap ( 5gt<cr>
nnoremap - 6gt<cr>

nnoremap <leader>nt :tabn<cr>
nnoremap <leader>pt :tabp<cr>

" hello world

" Tabs abbreviations
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>ct :tabclose<cr>
nnoremap <leader>to :tabonly<cr>

" Plugins

execute pathogen#infect()
syntax on
filetype plugin indent on

set termguicolors
execute "colorscheme horizon"

hi User1 guifg=White guibg=#6666ff
hi User2 guifg=#cccccc guibg=#4a4c71
hi User3 guifg=White guibg=#6666ff

inoremap sd <esc>:w<cr>:hi User1 guifg=White guibg=#8888ff<cr> 
vnoremap sd <esc>:w<cr>:hi User1 guifg=White guibg=#8888ff<cr> 

hi statusline guibg=#666666 guifg=#282a50

nnoremap v v:<BS><BS><BS><BS><BS>hi User1 guifg=White guibg=#c78463<cr>v

" Disabling Arrow keys
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

augroup bar
	autocmd!
	au InsertLeave * hi User1 guifg=White guibg=#8888ff
	au InsertEnter * hi User1 guifg=White guibg=#c65555
augroup end
