!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/vfloop.sh -cf $TR_SRC/c-f-pair.sh
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************

      subroutine decr (arg1)  bind(c)
        integer*4, VALUE :: arg1

        print*, 'F pre:  arg1=', arg1
        arg1 = arg1 - 1
        print*, 'F post: arg1=', arg1
      end subroutine
