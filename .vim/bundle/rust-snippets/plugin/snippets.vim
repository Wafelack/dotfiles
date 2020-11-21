if !exists("g:snips_path")
	let g:snips_path="$HOME/.vim/bundle/rust-snippets/snippets/"
endif
if !exists("g:rusty_map")
	let g:rusty_map="&"
endif

augroup rust
	autocmd!
	autocmd FileType rust execute "nnoremap <buffer> " . g:rusty_map . "test :-1read " . g:snips_path . ".test.rs<cr>"
	autocmd FileType rust execute "nnoremap <buffer> " . g:rusty_map . "struct :-1read " . g:snips_path . ".struct.rs<cr>"
	autocmd FileType rust execute "nnoremap <buffer> " . g:rusty_map . "trait :-1read " . g:snips_path . ".trait.rs<cr>"
	autocmd FileType rust execute "nnoremap <buffer> " . g:rusty_map . "main :-1read " . g:snips_path . ".main.rs<cr>"
	autocmd FileType rust execute "nnoremap <buffer> " . g:rusty_map . "enum :-1read " . g:snips_path . ".enum.rs<cr>"
	autocmd FileType rust execute "nnoremap <buffer> " . g:rusty_map . "match :-1read " . g:snips_path . ".math.rs<cr>"
augroup end
