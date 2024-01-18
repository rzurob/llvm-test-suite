!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/vfloop.sh
! %COMPOPTS: -qdebug=intmsg
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************

      recursive function sum (a)  bind(C, name='sumB')

        print*, 'myrv = ', a

        if (a <= 0.0) then
          sum = 0.0
        else
          sum = a + sum (a - 1.0)
        endif
      end function

      program p1
        interface
          recursive function sum (a)  bind(C, name='sumB')
          end function
        end interface

        rv = sum (3.0)
        print*, 'sum = ', rv
      end program;
