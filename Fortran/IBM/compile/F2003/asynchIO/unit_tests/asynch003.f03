       interface
         subroutine sub(arg)
           integer, asynchronous :: arg(3)
         end subroutine
       end interface
       integer :: i(10)
       call sub(i( (/2, 3, 4/)))
       end
