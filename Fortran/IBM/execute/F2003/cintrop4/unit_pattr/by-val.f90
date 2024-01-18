!*********************************************************************
!***********************************************************************

      subroutine decr (arg1)  bind(c)
        integer*4, VALUE :: arg1

        print*, 'F pre:  arg1=', arg1
        arg1 = arg1 - 1
        print*, 'F post: arg1=', arg1
      end subroutine
