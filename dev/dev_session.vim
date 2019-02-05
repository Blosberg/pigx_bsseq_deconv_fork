let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/projects/Cell_type_deconvolution/pigx_bsseq_deconv_fork
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +1 pigx-bsseq.in
badd +0 BSseq_pipeline.py
badd +32 ~/.vim/foldstyles.vim
badd +98 scripts/fetch_procedures.R
badd +0 scripts/func_defs.py
badd +0 config.json
badd +0 scripts/deconv_from_command_line_main.R
badd +0 scripts/deconv_funcs.R
argglobal
silent! argdel *
$argadd pigx-bsseq.in
$argadd BSseq_pipeline.py
set stal=2
edit pigx-bsseq.in
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winminheight=1 winheight=1 winminwidth=1 winwidth=1
wincmd =
argglobal
setlocal fdm=expr
setlocal fde=FoldPythonFuncdefs()
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 387 - ((342 * winheight(0) + 18) / 36)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
387
normal! 0
wincmd w
argglobal
2argu
setlocal fdm=expr
setlocal fde=FoldSnakemake()
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
23
normal! zo
51
normal! zo
201
normal! zo
let s:l = 222 - ((19 * winheight(0) + 18) / 36)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
222
normal! 029|
wincmd w
2wincmd w
wincmd =
tabedit scripts/func_defs.py
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winminheight=1 winheight=1 winminwidth=1 winwidth=1
wincmd =
argglobal
if bufexists('scripts/func_defs.py') | buffer scripts/func_defs.py | else | edit scripts/func_defs.py | endif
setlocal fdm=expr
setlocal fde=FoldPythonFuncdefs()
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 47 - ((23 * winheight(0) + 18) / 36)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
47
normal! 0
wincmd w
argglobal
if bufexists('config.json') | buffer config.json | else | edit config.json | endif
setlocal fdm=expr
setlocal fde=FoldPythonFuncdefs()
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 4 - ((3 * winheight(0) + 18) / 36)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
4
normal! 0
wincmd w
wincmd =
tabedit scripts/deconv_from_command_line_main.R
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winminheight=1 winheight=1 winminwidth=1 winwidth=1
wincmd =
argglobal
if bufexists('scripts/deconv_from_command_line_main.R') | buffer scripts/deconv_from_command_line_main.R | else | edit scripts/deconv_from_command_line_main.R | endif
setlocal fdm=expr
setlocal fde=FoldPythonFuncdefs()
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 45 - ((4 * winheight(0) + 18) / 36)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
45
normal! 0
wincmd w
argglobal
if bufexists('scripts/deconv_funcs.R') | buffer scripts/deconv_funcs.R | else | edit scripts/deconv_funcs.R | endif
setlocal fdm=expr
setlocal fde=RscriptFuncs()
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
1
normal! zo
let s:l = 38 - ((37 * winheight(0) + 18) / 36)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
38
normal! 01|
wincmd w
wincmd =
tabnext 1
set stal=1
if exists('s:wipebuf')
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToO
set winminheight=1 winminwidth=1
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
