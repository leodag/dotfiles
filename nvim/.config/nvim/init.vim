set ruler
set number
set relativenumber
set mouse=a

autocmd FocusLost * :set norelativenumber
autocmd FocusGained * :set relativenumber

autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

" space instead of tab
set expandtab
" tamanho de uma identação
set shiftwidth=4

" tamanho deslocado por um tab verdadeiro
set tabstop=4
" tamanho deslocado por um tab de espaços (0 = igual a tabstop)
set softtabstop=0

set noerrorbells

set splitbelow
set splitright

set scrolloff=3 " lines on top/at bottom when scrolling

set nostartofline " don't move to first character when jumping pages

set listchars=tab:>\ ,trail:-,extends:$,precedes:$,nbsp:+
set list

highlight ExtraWhitespace ctermbg=lightgrey
match ExtraWhitespace /\s\+$\|\t/

highlight Search ctermbg=green ctermfg=white

"highlight CursorLine cterm=reverse
"set cursorline

set clipboard=unnamedplus " use X secondary clipboard

