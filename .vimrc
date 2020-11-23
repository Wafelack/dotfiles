set number numberwidth=1 tabstop=4

let mapleader = "," " leader for commands
let maplocalleader = "!" " leader for local commands

nnoremap s <NOP>

" Completion options
set completeopt=menuone,longest

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
set statusline=\\|  " Displaying fancy beggining
set statusline+=\ %{toupper(g:currentmode[mode()])} " Displaying mode
set statusline+=\ \\|
set statusline+=\ %f " Displaying filename
set statusline+=\ \\|
set statusline+=\ %Y\ \\|\  " Displaying filetype
set statusline+=%{tolower(g:branch)}\ \\|
set statusline+=\ %m
set statusline+=%= " Switching to right side
set statusline+=\\|
set statusline+=\ Ln\ %l,Col\ %c " Displaying line number
set statusline+=\ \\|\ %p
set statusline+=%% " Displaying % symbol
set statusline+=\ \\|

" Remaping enter to select the current item in completion
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>" 

" Leaders remaping
nnoremap <leader>ev :tabfind<space>$MYVIMRC<cr>
noremap <leader>sv :source<space>$MYVIMRC<cr>:hi StatusLine ctermfg=Red ctermbg=White<cr>
nnoremap <leader>d dd
nnoremap <leader>c ddO
vnoremap <leader>" <esc>`<i"<esc>`>i"<esc>v
nnoremap <leader>" viw<esc>a"<esc>bi"<esc>lel
nnoremap <leader>' viw<esc>a'<esc>bi'<esc>lel
nnoremap <leader>i mmgg=G`m
nnoremap <leader>f :call ToggleTabSize()<cr>
nnoremap <cr> $i<Right><cr><esc>

" Remaping without leaders
inoremap sd <esc>:w<cr>:hi statusline guibg=White guifg=#6c6f93<cr>
vnoremap sd <esc>:w<cr>:hi statusline guibg=White guifg=#6c6f93<cr>

" Uppercase word
nnoremap <c-u> wvbu
" Delete current line
nnoremap <c-d> dd
" Select the whole buffer
nnoremap <c-a> ggvG$
vnoremap <c-a> <esc>ggvG$
" Increase or dicrease panel size
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

" Tabs abberviations
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>ct :tabclose<cr>
nnoremap <leader>to :tabonly<cr>

" Plugins

execute pathogen#infect()
syntax on
filetype plugin indent on

set termguicolors
execute "colorscheme horizon"

hi statusline guibg=White guifg=#6c6f93

nnoremap v v:<BS><BS><BS><BS><BS>hi statusline guibg=White guifg=#c78463<cr>v

augroup bar
	autocmd!
	au InsertLeave * hi statusline guibg=White guifg=#6c6f93
	au InsertEnter * hi statusline guibg=White guifg=#fa1235
augroup end
