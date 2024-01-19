       ! C(1232)
       subroutine sub(arg)
         interface
           subroutine sub2(arg2)
             integer, asynchronous :: arg2(10)
           end subroutine
         end interface
         integer :: arg(:)
         call sub2(arg)
       end subroutine
