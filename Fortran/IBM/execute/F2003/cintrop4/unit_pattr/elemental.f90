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

      elemental function f1 (a1)  bind(c)
        intent(in) :: a1

        f1 = a1 + 1
      end function


      program main
        dimension a(5)

        a = 1
        print*, a

        a = f1 (a)
        print*, a
      end
