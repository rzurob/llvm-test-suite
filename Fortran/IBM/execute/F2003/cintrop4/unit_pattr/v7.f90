!*********************************************************************
!***********************************************************************

  !!! C function (one arg) called from Fortran via NAME= binding label
  !!! containing mixed case and surrounded by blanks.

  PROGRAM prog1
    INTERFACE  ! interface for non-void C function
      FUNCTION f1 (arg1) BIND(C, NAME="  Func1 ")
        INTEGER f1
        INTEGER, VALUE :: arg1
      END FUNCTION f1
    END INTERFACE

    print *, 'f1 returned', f1 (3)
  END PROGRAM

  ! Expected result:
  ! hello
  !  f1 returned 4
