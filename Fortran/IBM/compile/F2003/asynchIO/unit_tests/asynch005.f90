       ! C1233
       interface
         subroutine sub2(arg2)
           integer, asynchronous :: arg2(10)
         end subroutine
       end interface
       integer,pointer :: arg(:)
       call sub2(arg)
       end
