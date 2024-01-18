!*********************************************************************
!***********************************************************************

  !!! Fortran ENTRY in subroutine (one arg by value) called from C.

  SUBROUTINE s1  ! no BIND(C)
    INTEGER, VALUE :: arg1

    print*, 'hello'

  ENTRY s1e1 (arg1) BIND(C)
    print*, 'arg1 is ', arg1

  END SUBROUTINE s1
