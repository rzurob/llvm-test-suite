!*********************************************************************
!***********************************************************************

  !!! C function (no args, no return value) called from Fortran.

  PROGRAM prog1
    INTERFACE  ! interface equivalent to C function prototype `void f1 (void);'
      SUBROUTINE f1 () BIND(C)
      END SUBROUTINE f1
    END INTERFACE

    CALL f1
  END PROGRAM
