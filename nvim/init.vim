call plug#begin()

Plug 'chaoren/vim-wordmotion'
Plug 'machakann/vim-sandwich'

call plug#end()

PlugInstall

command! ReloadInit source ~/AppData/Local/nvim/init.vim

set clipboard^=unnamed

if exists('g:vscode')
    xmap gc  <Plug>VSCodeCommentary
    nmap gc  <Plug>VSCodeCommentary
    omap gc  <Plug>VSCodeCommentary
    nmap gcc <Plug>VSCodeCommentaryLine
endif
