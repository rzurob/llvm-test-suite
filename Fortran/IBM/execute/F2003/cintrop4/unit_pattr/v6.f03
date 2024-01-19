!*********************************************************************
!***********************************************************************

  !!! Fortran function (one arg not-by-value) called from C.

  FUNCTION f1 (arg1) BIND(C) RESULT(f1r1)
    INTEGER f1r1  ! function 1, result 1
    INTEGER arg1  ! arg1 is a C pointer to integer

    f1r1 = arg1 + 1
  END FUNCTION f1
