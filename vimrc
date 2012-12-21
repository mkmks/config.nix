syntax enable
filetype plugin indent on

set hidden
set nobackup
set nowb
set noswapfile

set hlsearch
set smartcase
set wildmode=longest:full
set wildmenu
set history=1000
set relativenumber

set laststatus=2                             " always show statusbar  
set statusline=  
set statusline+=%-10.3n\                     " buffer number  
set statusline+=%f\                          " filename   
set statusline+=%h%m%r%w                     " status flags  
set statusline+=\[%{strlen(&ft)?&ft:'none'}] " file type  
set statusline+=%=                           " right align remainder  
set statusline+=%-14(%l,%c%V%)               " line, character  
set statusline+=%<%P                         " file position  

set tags=./tags,./../tags,./*/tags,~/src/**/tags

set expandtab
set shiftwidth=2
set softtabstop=2
set autoindent
