"au Filetype java set makeprg=javac\ -d\ ~/java/svetdrd/classes/\ %
autocmd FileType java let g:java_classpath = '/home/jl/progr/java/yadb/build'
map <F9> :!ant compile<CR>
map <F5> :!ant run<CR>

:!ctags -R .

let g:buildFile = '/home/jl/progr/java/yadb/build.xml'
"let g:antOption = '-debug'
