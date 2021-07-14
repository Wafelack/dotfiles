"| Line numbers
set number " Show line numbers
set numberwidth=4 " Line number zone is at least 4 chars wide
augroup numbers " Enable or not relative line numbers depending on the mode
    autocmd!
    au InsertEnter * set norelativenumber
    au InsertLeave * set relativenumber
augroup end

"| Files created by VIM
" Source control replaces that.
set noswapfile 
set noundofile 
set nobackup 
set nowritebackup 
" Remove useless and annoying logging files.
set viminfo=

"| Files browsing/search and completion
set completeopt=menuone " Show completion popup even if there is one match.
set path+=** " Enable fuzzy path search.
set wildmenu " Improve completion menu.
let g:netrw_banner=0 " Disable annoying banner.
let g:netrw_browse_split=0 " Open the file in the same window as the file explorer.
let g:netrw_liststyle=3 " Filesystem tree view.

"| Readbility
set scrolloff=15 " Absolute minimum to see the code around.
set noshowmode " Status bar replaces it.
set termguicolors " Enable GUI colors
set tabstop=2 " Set tab width.
set shiftwidth=2
set expandtab " Replace tabs with spaces.
syntax enable " Enable syntax items.

"| Keymaps
let mapleader=","
nnoremap <leader>sv :source<space>$MYVIMRC<cr>

"| Linter
set omnifunc=ale#completion#OmniFunc
let g:ale_completion_enabled = 1
let g:ale_sign_column_always = 1
let g:ale_fix_on_save = 1
let g:ale_linters = { 'rust' : [ 'analyzer' ] }

" Projet specific vimrc
set exrc
set secure

"| Plugins

" Vim plug auto install
if empty(glob('~/.vim/autoload/plug.vim'))
  silent execute '!curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged') "| Setup plugins directory.
Plug 'jiangmiao/auto-pairs' "| Symbol pairing
Plug 'luochen1990/rainbow'  "| Symbol highlighting
Plug 'dense-analysis/ale' "| Linter
Plug 'arcticicestudio/nord-vim' "| Theme
call plug#end()

"| Fancy

colorscheme nord
let g:nord_cursor_line_number_background = 1
let g:nord_italic_comments = 1
filetype plugin on
let g:rainbow_active = 1 " Enable symbol highlighting.
set laststatus=2
let modes = {
            \ 'n' : 'NORMAL',
            \ 'i' : 'INSERT',
            \ 'ic': 'COMPLETE',
            \ 'ix': 'COMPLETE',
            \ 'R' :'REPLACE',
            \ 'Rc': 'R-COMPLETE',
            \ 'Rv': 'V-REPLACE',
            \ 'Rx': 'R-COMPLETE',
            \ 'c' : 'COMMAND',
            \ 'cv': 'EXECUTE',
            \ 'ce': 'EXECUTE',
            \ 'r' : 'PROMPT',
            \ 'rm': 'MORE',
            \ 'r?': 'CONFIRM',
            \ '!' : 'SHELL',
            \ 't' : 'TERMINAL',
            \ 'v' : 'VISUAL',
            \ 'V' : 'VLINE',
            \ '': 'VBLOCK',
            \ 's' : 'SELECT',
            \ 'S' : 'SLINE',
            \ '': 'SBLOCK'}
function! GitBranch()
    return system("git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -d '\n'")
endfunction
function! StatusBranch()
    let l:branch = GitBranch()
    if strlen(l:branch) > 0
        return '/  ' . l:branch
    else
        return ''
    endif
endfunction
set statusline=\ %{modes[mode()]}\ /
set statusline+=\ %f
set statusline+=\ %{StatusBranch()}
set statusline+=\ %m
set statusline+=%=\ %y\ \\
set statusline+=\ %{&fileencoding?&fileencoding:&encoding}\ \\
set statusline+=\ %p%%\ %l/%L:%c\ 
