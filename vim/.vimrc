" Name: Wafelack's vimrc
" File: ~/.vimrc
" Author: Wafelack <wafelack@riseup.net>
" License: GNU General Public License version 3.0 or any later version.

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

set tabstop=4
set shiftwidth=4
set expandtab smarttab nowrap
set ai si

"}}}

"{{{Status bar

let modes = {
            \ 'n' : 'N',
            \ 'i' : 'I',
            \ 'ic': 'IC',
            \ 'ix': 'IX',
            \ 'R' : 'R',
            \ 'Rc': 'RC',
            \ 'Rx': 'RX',
            \ 'Rv': 'RC',
            \ 'c' : 'C',
            \ 'cv': 'CV',
            \ 'ce': 'CE',
            \ 'r' : 'R',
            \ 'rm': 'RM',
            \ 'r?': 'R?',
            \ '!' : 'S',
            \ 'v' : 'V',
            \ 'V' : 'VL',
            \ '': 'VB',
            \ 's' : 'S',
            \ 'S' : 'SL',
            \ '': 'SB',
            \}

function! GitBranch()
    return system('git rev-parse --abbrev-ref HEAD 2> /dev/null | tr -d "\n"') 
endfunction

function! StatusBranch()
    let l:branch = GitBranch()
    if strlen(l:branch) > 0
        return l:branch
    else
        return ''
    endif
endfunction

set laststatus=2
set statusline=\ %{modes[mode()]}\ \|
set statusline+=\ %f
set statusline+=\ (%F)
set statusline+=\ %m
set statusline+=\ %=
set statusline+=\ %{StatusBranch()}
set statusline+=\ %y
set statusline+=\ %l\ %c\ %p%%

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
let g:netrw_browse_split = 4
let g:netrw_list_style = 3
let g:netrw_dirhistmax = 0
let g:netrw_list_hide = netrw_gitignore#Hide()

"}}}

"{{{Mappings and Abbrevs

let mapleader = ' '
nnoremap <leader>bk :x<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>
nnoremap <leader>t  :vs .<CR>65<
nnoremap <leader>dt :tabclose<CR>
nnoremap <leader>nt :tabnew<CR>
nnoremap <leader>bs :source %<CR>
nnoremap <leader>ds :call DeleteSexpr()<CR>
nnoremap <C-f> :silent exec "!cargo fmt >& /dev/null"<CR>:exec "redraw!"<CR>
nnoremap <C-x>q :qa!<CR>
" Jump to help page
nnoremap gh T\|yt\|:silent! exec ":help " . @"<CR>

"}}}

"{{{Backups

set noswapfile noundofile
set nobackup nowritebackup
set undofile
set undodir=/tmp/
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

augroup tags
    autocmd!
    autocmd BufWritePost * 
                \ if !empty(glob("tags")) 
                \ | silent exec "!ctags -R ."
                \ | endif
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

"{{{Make and quickfixlist

augroup compilers
    autocmd!
    autocmd BufNewFile,BufRead *.rs
        \ setlocal makeprg=cargo
        " Borrowed from https://github.com/rust-lang/rust.vim
        \ setlocal errorformat=
            \%-G,
            \%-Gerror:\ aborting\ %.%#,
            \%-Gerror:\ Could\ not\ compile\ %.%#,
            \%Eerror:\ %m,
            \%Eerror[E%n]:\ %m,
            \%Wwarning:\ %m,
            \%Inote:\ %m,
            \%C\ %#-->\ %f:%l:%c,
            \%E\ \ left:%m,%C\ right:%m\ %f:%l:%c,%Z
        \ setlocal errorformat+=
            \%f:%l:%c:\ %t%*[^:]:\ %m,
            \%f:%l:%c:\ %*\\d:%*\\d\ %t%*[^:]:\ %m,
            \%-G%f:%l\ %s,
            \%-G%*[\ ]^,
            \%-G%*[\ ]^%*[~],
            \%-G%*[\ ]...
        \ setlocal errorformat+=
            \%-G%\\s%#Downloading%.%#,
            \%-G%\\s%#Compiling%.%#,
            \%-G%\\s%#Finished%.%#,
            \%-G%\\s%#error:\ Could\ not\ compile\ %.%#,
            \%-G%\\s%#To\ learn\ more\\,%.%#,
            \%-Gnote:\ Run\ with\ \`RUST_BACKTRACE=%.%#,
            \%.%#panicked\ at\ \\'%m\\'\\,\ %f:%l:%c
    autocmd BufNewFile,BufRead *.c,*.cc,*.cpp,*.cxx setlocal makeprg=make
augroup end

"}}}

"{{{Snippets

iabbrev cmain int<CR>main (int argc, char * const argv[])<CR>{<CR>  <CR>}<ESC>k$i

"}}}

"{{{Plugins

execute pathogen#infect('plugin/{}')
syntax enable
filetype plugin indent on

"}}}

"{{{Fancy

colorscheme pastry
let g:rainbow_active = 1

"}}}
