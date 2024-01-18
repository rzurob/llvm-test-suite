! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/runf.sh fxbind_c03opa
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan, 7, 2004
!* ORIGIN                       : AIX Complier Development
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
!*   - External procedure ( Fun/Entry), the interface is implicit.
!*   - Primary entry point have bind(c) attribute while
!*     alternate entry point do not have bind(c) attribute.
!*   - datatype: Integer.
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
  INTEGER :: a = 1, b = 2, c = 1, d = 2
  INTEGER :: I,result
  logical :: test
  CALL initl ( a, b, c, d )
  I = 2
  CALL eval3 (i, result )
  test = result .EQ. 25
  call assert(test,'The result is not correct',9)

END PROGRAM testentry

FUNCTION eval3 ( x, result ) BIND(C) ! Global-Scope function with bind(c)
  !
  !     Evaluates a third order polynomial of the form:
  !        RESULT = A + B*X + C*X**2 + D*X**3
  !
  !     Declare calling arguments
  IMPLICIT NONE
  INTEGER :: a1, b1, c1, d1, x, result
  INTEGER :: eval3,initl

  ! Declare local variables
  ! The SAVE attribute forces the program
  ! to retain the value of a procedure variable
  ! from one call to the next.
  INTEGER, SAVE :: a, b, c, d

  ! Calculate result
  result = a + b**x + c*x**2 + d*x**3
  eval3 = result
  RETURN

  ! Entry INITL specifies the values of a, b, c, and d
  ! to be used when evaluating the polynomial.

  ENTRY initl(a1, b1, c1, d1) !Global-Scope function-entry with bind(c)
  a = a1
  b = b1
  c = c1
  d = d1
  initl =  a1
  RETURN
END FUNCTION eval3
