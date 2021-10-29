" Name: Wafelack's vimrc
" File: ~/.vimrc
" Author: Wafelack <wafelack@riseup.net>
" License: GNU General Public License version 3.0 or any later version.

let $MYVIMRC = $HOME . "/.dotfiles/vim/.vimrc"

"{{{Line numbers

" Show line numbers
set number 
set numberwidth=4
set relativenumber
augroup numbers
	autocmd!
	au InsertEnter * set norelativenumber
	au InsertLeave * set relativenumber
augroup end

"}}}

"{{{Indentation

set noexpandtab

"}}}

"{{{Status bar

function! FormatMode(mode)
	let modes = {
			\ 'n' : 'NORMAL',
			\ 'i' : 'INSERT',
			\ 'ic': 'INSERT-COMPL',
			\ 'ix': 'INSERT-COMPL',
			\ 'R' : 'REPLACE',
			\ 'Rc': 'REPLACE-COMPL',
			\ 'Rx': 'REPLACE-COMPL',
			\ 'Rv': 'VIRTUAL-REPLACE',
			\ 'c' : 'COMMAND',
			\ 'cv': 'VIM-EX',
			\ 'ce': 'NORMAL-EX',
			\ 'r' : 'PROMPT',
			\ 'rm': 'MORE',
			\ 'r?': 'CONFIRM',
			\ '!' : 'SHELL',
			\ 'v' : 'VISUAL',
			\ 'V' : 'VISUAL-LINE',
			\ '': 'VISUAL-BLOCK',
			\ 's' : 'SELECT-CHAR',
			\ 'S' : 'SELECT-LINE',
			\ '': 'SELECT-BLOCK',
			\}
	return modes[a:mode]
endfunction


function! GitBranch()
	return system('git rev-parse --abbrev-ref HEAD 2> /dev/null | tr -d "\n"') 
endfunction

function! StatusBranch()
	let l:branch = GitBranch()
	if strlen(l:branch) > 0
		return '[' . l:branch . ']'
	else
		return ''
	endif
endfunction

highlight User1 ctermbg=12 ctermfg=15

set laststatus=2
set statusline=%1*\ %{FormatMode(mode())}\ %* " Formatted edition mode.
set statusline+=\ %f " File name.
set statusline+=\ %{StatusBranch()} " Git branch.
set statusline+=\ %m " Modified flag.
set statusline+=\ %=
set statusline+=\ %{&ft} " Language.
set statusline+=\ [%{&ff}#%{&fileencoding}]
set statusline+=%1*\ %p%%\ ::\ %l/%L\ :\ %c\ %* " Line:Col Percentage


"}}}

"{{{Readability and handiness

set completeopt=menuone
set path+=** """ Fuzzy search
set wildmenu
set scrolloff=15
set noshowmode
set showcmd
set cursorline

set showmatch
set incsearch

"}}}

"{{{File Browsing

let g:netrw_banner = 0
let g:netrw_browse_split = 0
let g:netrw_list_style = 3
let g:netrw_dirhistmax = 0
let g:netrw_list_hide = netrw_gitignore#Hide() . ',.*\.o'
let g:netrw_winsize = 25

"}}}

"{{{Sessions

let g:session_file = "~/.vimsession"

augroup session
	autocmd!
	autocmd VimLeave * execute "mksession! " . g:session_file
augroup end

"}}}

"{{{Sourcing

function! SourceFolder(folder)
	for file in split(globpath(len(a:folder) > 0 ? a:folder : '.', '**'), '\n')
		if file =~ '.*\.vim'
			echom "[+] Sourcing `" . file . "`."
			execute "source " . file
		endif
	endfor
endfunction

"}}}

"{{{Mappings and Abbrevs

let mapleader = ' '
nnoremap <leader>bk :x<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>
nnoremap <leader>sc :source %<CR>
nnoremap <leader>sf :call SourceFolder(input("Folder to source (default: `.`): "))<CR>
nnoremap <leader>sl :call SourceFolder('./autoload/')<CR>
nnoremap <leader>t  :Vexplore<CR>
nnoremap <leader>dt :tabclose<CR>
nnoremap <leader>nt :tabnew<CR>
nnoremap <leader>bs :source %<CR>
nnoremap <leader>ds :call DeleteSexpr()<CR>
nnoremap <C-x>q :qa!<CR>
" Jump to help page
nnoremap gh T\|yt\|:silent! exec ":help " . @"<CR>

nnoremap <C-q>c :cclose<CR>
nnoremap <C-q>o :copen<CR>
nnoremap <C-q>n :cn<CR>
nnoremap <C-q>p :cp<CR>
nnoremap <leader>l :execute ("source" . g:session_file)<CR>

"}}}

"{{{Backups

set noswapfile noundofile
set nobackup nowritebackup
set undofile
set undodir=$HOME/.vim_undo_files
set viminfo=

"}}}

"{{{ Presentation mode 

" Borrowed and modified from https://github.com/mcantor/no_plugins.
augroup presentation
	autocmd!
	autocmd BufNewFile,BufRead *.vimp setfiletype vim
	autocmd BufNewFIle,BufRead *.vimp setl window=66
	autocmd BufNewFile,BufRead *.vimp normal 17Gzz
	autocmd BufNewFile,BufRead *.vimp command! GO normal M45jzzH
	autocmd BufNewFile,BufRead *.vimp command! BACK normal M45kzzH
	autocmd BufNewFile,BufRead *.vimp command! RUN execute getline(".")
	autocmd BufNewFile,BufRead *.vimp nnoremap <buffer> <C-f> :GO<CR>
	autocmd BufNewFile,BufRead *.vimp nnoremap <buffer> <C-b> :BACK<CR>
augroup end

"}}}

"{{{Tags

function! RegenTags(root)
	if !empty(glob('tags'))
		silent exec '!ctags -R ' . a:root
	endif
endfunction

" Assume we use Universal Ctags
augroup tags
	autocmd!
	autocmd FileType rust let g:tags_root = './src/'
	autocmd FileType c,cpp let g:tags_root = './sources/'
	autocmd FileType lisp let g:tags_root = './'

	autocmd BufWritePost *.rs call RegenTags('./src/')
	autocmd BufWritePost *.cl,*.lisp call RegenTags('./')
	autocmd BufWritePost *.c,*.cc,*.cpp,*.cxx,*.h,*.hh,*.hpp call RegenTags('./sources/')
augroup end

"}}}

"{{{S-Expressions

function! GetChar()
	return matchstr(getline('.'), '\%'.col('.').'c.')
endfunction

function! DeleteSexpr()
	if GetChar() == "("
		normal da(
	elseif GetChar() == "\""
		normal da"
	elseif GetChar() == "'"
		normal x
		call DeleteSexpr()
	elseif GetChar() != ")"
		normal h
		if GetChar() != "(" && GetChar() != "'" && GetChar() != " "
			normal lb
		else
			normal l
		endif
		normal dw
	endif
endfunction

"}}}

"{{{QuickfixList and :make

augroup quickfix
	autocmd!
	" Stole part of that formatter from rust-lang/rust.vim. (But
	" please do not use it, this plugin is overall bloated, just
	" use the quickfix list.)
	autocmd FileType rust
				\ let &efm = '%E-->\ %f:%l:%c: %\d%#:%\d%# %.%\{-}error:%.%\{-} %m,' . '%W-->\ %f:%l:%c: %\d%#:%\d%# %.%\{-}warning:%.%\{-} %m,' . '%C-->\ %f:%l %m' . ',' . '%-G,' . '%-Gerror: aborting %.%#,' . '%-Gerror: Could not compile %.%#,' . '%Eerror: %m,' . '%Eerror[E%n]: %m,' . '%-Gwarning: the option `Z` is unstable %.%#,' . '%Wwarning: %m,' . '%Inote: %m,' . '%C %#--> %f:%l:%c'
				\ | if !empty(glob("Cargo.toml"))
					\   | setlocal makeprg=cargo
					\ | else
						\   | setlocal makeprg=rustc
						\ | endif
	autocmd FileType haskell
				\ if !empty(glob("stack.yaml"))
				\   | setlocal makeprg=stack
				\ | elseif !empty(glob("*.cabal"))
					\   | setlocal makeprg=cabal
					\ | else
						\   | setlocal makeprg=ghc
						\ | endif
	autocmd FileType c
				\   if !empty(glob("makefile")) || !empty(glob("Makefile"))
				\   | setlocal makeprg=make
				\ | else
					\   | setlocal makeprg=cc
					\ | endif

	autocmd QuickFixCmdPost make copen
augroup end

"}}}

"{{{Snippets

function! GetScreamingSnakeBasename(str)
	let l:relativel = split(a:str, '/')
	if l:relativel[0] == 'sources' || l:relativel[0] == 'src'
		let l:relativel = l:relativel[1:]
	endif
	return '_' . substitute(toupper(join(l:relativel, '_')), '\.', '_', 'g')
endfunction

function! WriteHeaderCode()
	let l:name = GetScreamingSnakeBasename(fnamemodify(expand("%"), ":~:."))
	execute "normal! i#ifndef " . l:name . "\<CR>"
	execute "normal! i# define " . l:name . " 1\<CR>"
	execute "normal! i\<CR>\<CR>\<CR>#endif /* " . l:name . " */"
	execute "normal! kk"
endfunction

augroup snippets
	autocmd!
	autocmd BufNewFile *.h call WriteHeaderCode()
augroup end

"}}}

"{{{Language specific indentation

augroup indentation
	autocmd!
	autocmd BufNewFile,BufRead *.c,*.cpp,*.cc,*.cxx,*.h,*.hh,*.hpp set noexpandtab tabstop=8 shiftwidth=8 softtabstop=8
	autocmd BufNewFile,BufRead *.scm,*.lisp set expandtab tabstop=2 shiftwidth=2 softtabstop=2
	autocmd BufNewFile,BufRead *.rs set noexpandtab tabstop=4 shiftwidth=4 softtabstop=4
augroup end

"}}}

"{{{Cursed

function! Trigraphy()
	%s/\[/\?\?\(/ " [ = ??(
	%s/\]/\?\?\)/ " ] = ??)
	%s/\{/\?\?\</ " { = ??<
	%s/\}/\?\?\>/ " } = ??>
	%s/\\/\?\?\// " \ = ??/
	%s/\^/\?\?\'/ " ^ = ??'
	%s/\#/\?\?\=/ " # = ??=
	%s/|/\?\?\!/  " | = ??!
	%s/\~/\?\?\-/ " ~ = ??-
endfunction

"}}}

"{{{Plugins

syntax enable
filetype plugin indent on

call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'luochen1990/rainbow'
Plug 'ackyshake/VimCompletesMe'

call plug#end()

"}}}

"{{{Fancy

colorscheme pastry

let g:rainbow_active = 2

set list
set listchars=tab:>—,eol:¬,trail:\ ,nbsp:¤
set fillchars=vert:\ 

"}}}

"{{{EMACS keybindings, because it is fun

nnoremap <Esc>x :exec input("M-x ", "", "command")<CR>
command! -nargs=* Compile make <args>
cnoreabbrev compile Compile

nnoremap <C-x><C-f> :e .<CR>
nnoremap <C-x><C-c> :qa<CR>

"}}}

"{{{Completion

set omnifunc=syntaxcomplete#Complete

"}}}

"{{{Custom Filetypes

augroup ftypes
	autocmd!
	autocmd BufNewFile,BufRead *.asd setfiletype lisp
augroup end

"}}}
