set number numberwidth=1 tabstop=4
colorscheme desert

let mapleader = "," " leader for commands
let maplocalleader = "!" " leader for local commands

set path+=** " Getting fuzzy
set wildmenu

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
						\}


augroup colors
		autocmd!
		au BufEnter * hi StatusLine ctermfg=Red ctermbg=White
		au InsertEnter * hi StatusLine ctermfg=Green ctermbg=White
		au InsertLeave * hi StatusLine ctermfg=Red ctermbg=White
augroup end

set laststatus=2 " Always display a status line
set statusline=--\  " Displaying fancy beggining
set statusline+=[
set statusline+=%{toupper(g:currentmode[mode()])} " Displaying mode
set statusline+=]
set statusline+=\ %f " Displaying filename
set statusline+=\ (%Y) " Displaying filetype
set statusline+=\ %m " Displaying [+] if the file is modified
set statusline+=%= " Switching to right side
set statusline+=%l,%c\ -\ %p " Displaying line number
set statusline+=%% " Displaying % symbol
set statusline+=\ \ -- " Displaying fancy end

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
" Comment current line
nnoremap // 0i// <space><esc>
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

function! OpenFolderInRightPane()
		:tabnew .	
endfunction

" Coding abbreviations
iabbrev { {}<Left>

" Tabs
nnoremap & 1gt<cr>
nnoremap é 2gt<cr>
nnoremap " 3gt<cr>
nnoremap ' 4gt<cr>
nnoremap ( 5gt<cr>
nnoremap - 6gt<cr>

" Remaping Ctrl + space for autocomplete
inoremap <c-@> <C-n> 

nnoremap <leader>nt :tabn<cr>
nnoremap <leader>pt :tabp<cr>

" Tabs abberviations
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>ct :tabclose<cr>
nnoremap <leader>to :tabonly<cr>

" Plugins
"
" Used plugins :
" - NERDTree
" - markdown-vim

execute pathogen#infect()
syntax on
filetype plugin indent on
