!*********************************************************************
!***********************************************************************

  !!! Simple Fortran subroutine (no args) called from C.

  SUBROUTINE s1 () BIND(C)
    print*, 'hello'
  END SUBROUTINE s1
