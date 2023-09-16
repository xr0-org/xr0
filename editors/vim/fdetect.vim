au Bufread,BufNewFile *.x, set filetype=xr0

au BufRead,BufNewFile *.h if search('axiom\|sfunc\|lemma', 'nw') | set filetype=xr0 | endif
