"let myvar=system('pwd')
"let dirname=$myvar . '/cscope_source.out'
"echo $dirname
silent cs add /home/ericksun/program/rabbitmq-server/deps/amqp_client/cscope_source.out
"silent cs add $myvar
"silent !find -name "*.erl" -o -name "*.hrl" |xargs ctags -R


if has("cscope")
            set cscopetag   " 使支持用 Ctrl+]  和 Ctrl+t 快捷键在代码间跳来跳去
            " check cscope for definition of a symbol before checking ctags:
            " set to 1 if you want the reverse search order.
             set csto=1

             " add any cscope database in current directory
             "if filereadable("cscope.out")
             "    cs add cscope.out
             "" else add the database pointed to by environment variable
             "elseif $CSCOPE_DB !=""
             "    cs add $CSCOPE_DB
             "endif

         "if filereadable("/home/p_jdzxsun/workspace/cscope.out")
             "    cs add /home/p_jdzxsun/workspace/cscope.out
             "endif

             " show msg when any other cscope db added
             set cscopeverbose

             nmap <C-/>s :cs find s <C-R>=expand("<cword>")<CR><CR>
             nmap <C-/>g :cs find g <C-R>=expand("<cword>")<CR><CR>
             nmap <C-/>c :cs find c <C-R>=expand("<cword>")<CR><CR>
             nmap <C-/>t :cs find t <C-R>=expand("<cword>")<CR><CR>
             nmap <C-/>e :cs find e <C-R>=expand("<cword>")<CR><CR>
             nmap <C-/>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
             nmap <C-/>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
             nmap <C-/>d :cs find d <C-R>=expand("<cword>")<CR><CR>
endif

