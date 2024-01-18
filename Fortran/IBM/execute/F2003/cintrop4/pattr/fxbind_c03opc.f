! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan, 7, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: Fun/Entry with BINC(C) attribute .
!*   - FORTRAN code only , the interoperable function is implemented
!*     in Fortran and called in Fortran.
!*   - External procedure ( Fun/Entry), the interfaces is implicit.
!*   - Primary entry point have  bind(c) attribute while  alternate
!*     entry point do not have bind(c) attribute.
!*   - Datatype :complex
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM testentry
  use assertmod
  IMPLICIT NONE
  INTEGER :: I
  COMPLEX(4) :: a = (10.0e0,10.0e0),b = (5.0e0,5.0e0)
  COMPLEX(4) :: result,compare_vaule = (20.0e0,20.0e0)
  logical :: precision_R4,test
  CALL initl ( a, b)
  I = 2
  CALL eval3 (i, result )
  test = precision_R4(result,compare_vaule)
  call assert(test,'The result is not correct',9)
END PROGRAM testentry

FUNCTION eval3 ( x, result ) BIND(C) ! Global-Scope function with bind(c)
  !
  !     Evaluates a polynomial of the form:
  !        RESULT = A + B*X
  !
  !     Declare calling arguments
  IMPLICIT NONE
  INTEGER :: x
  COMPLEX(4) :: a1, b1,result
  COMPLEX(4) :: eval3,initl

  ! Declare local variables
  ! The SAVE attribute forces the program
  ! to retain the value of a procedure variable
  ! from one call to the next.
  COMPLEX(4), SAVE :: a, b

  ! Calculate result
  result = a + b*x

  RETURN

  ! Entry INITL specifies the values of a and b
  ! to be used when evaluating the polynomial.

  ENTRY initl(a1, b1) !Global-Scope function-entry
  a = a1
  b = b1

  RETURN
END FUNCTION eval3
