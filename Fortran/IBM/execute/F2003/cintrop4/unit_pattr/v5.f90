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

  !!! Fortran ENTRY in function (no args) called from C, both declared BIND(C).

  FUNCTION f1 (arg1) BIND(C)
    INTEGER f1, f1e1  ! function 1, entry 1
    INTEGER, VALUE :: arg1

    f1 = arg1 + 1
    RETURN

  ENTRY f1e1 () BIND(C)
    f1e1 = 1

  END FUNCTION f1
