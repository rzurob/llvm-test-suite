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

  !!! Simple Fortran subroutine (no args) called from C.

  SUBROUTINE s1 () BIND(C)
    print*, 'hello'
  END SUBROUTINE s1
