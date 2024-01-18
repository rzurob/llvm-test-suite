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

  !!! C function (no args, no return value) called from Fortran.

  PROGRAM prog1
    INTERFACE  ! interface equivalent to C function prototype `void f1 (void);'
      SUBROUTINE f1 () BIND(C)
      END SUBROUTINE f1
    END INTERFACE

    CALL f1
  END PROGRAM
