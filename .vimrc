set number numberwidth=1 tabstop=4
colorscheme desert

let mapleader = "," " leader for commands
let maplocalleader = "!" " leader for local commands

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



augroup colors
		autocmd!
		au BufEnter * let g:branch = substitute(system('git branch --show-current'), '\n', '', 'g')
		au InsertEnter * let g:branch = substitute(system('git branch --show-current'), '\n', '', 'g')
		au InsertLeave * let g:branch = substitute(system('git branch --show-current'), '\n', '', 'g')
		au BufEnter * hi TabLine ctermfg=White ctermbg=Grey
		au BufEnter * hi TabLineSel ctermfg=White ctermbg=DarkGrey
		au BufEnter * hi StatusLine ctermfg=Red ctermbg=White
		au BufEnter * hi TabLineFill ctermfg=Grey ctermbg=Grey
		au InsertEnter * hi StatusLine ctermfg=Green ctermbg=White
		au InsertLeave * hi StatusLine ctermfg=Red ctermbg=White
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
set statusline+=%= " Switching to right side
set statusline+=\\|
set statusline+=\ Ln\ %l,Col\ %c " Displaying line number
set statusline+=\ \\|\ %p
set statusline+=%% " Displaying % symbol
set statusline+=\ \\|

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

" Remaping without leaders
inoremap sd <esc>:w<cr>
vnoremap sd <esc>:w<cr>
" Other maps
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

" abbreviations
iabbrev @@ wafelack@protonmail.com
iabbrev web https://wafelack.fr

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
