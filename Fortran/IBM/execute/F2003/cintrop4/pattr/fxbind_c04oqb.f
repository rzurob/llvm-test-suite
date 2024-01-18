! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/runf.sh fxbind_c04oqb 
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
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxbind_c04oqb.f
!* TEST CASE TITLE              : BIND(C) for Fortran procedures 
!*
!* PROGRAMMER                   : Kan Tian
!* DATE                         : Jan, 7, 2004
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions
!*                               contained in module.
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: Fun/Entry with BINC(C) attribute .
!*   - FORTRAN code only , the interoperable function is implemented
!*     in Fortran and called in Fortran.
!*   - External procedure ( Fun/Entry), the interfaces is implicit.
!*   - Primary entry point do not have bind(c) attribute while 
!*     an alternate entry point have bind(c) attribute.
!*   - Datatype :real
!*   
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module meval
contains

FUNCTION eval3 ( x, result ) ! Global-Scope function 
  !
  !     Evaluates a third order polynomial of the form:
  !        RESULT = A + B*X + C*X**2 + D*X**3
  !
  !     Declare calling arguments
  IMPLICIT NONE
  REAL :: a1, b1, c1, d1, x, result
  REAL :: eval3,initl

  ! Declare local variables
  ! The SAVE attribute forces the program
  ! to retain the value of a procedure variable
  ! from one call to the next.
  REAL, SAVE :: a, b, c, d

  ! Calculate result
  result = a + b**x + c*x**2 + d*x**3 
  eval3 = a
  RETURN

  ! Entry INITL specifies the values of a, b, c, and d
  ! to be used when evaluating the polynomial.

  ENTRY initl(a1, b1, c1, d1)  BIND(C)  !Global-Scope function-entry 
  a = a1
  b = b1
  c = c1
  d = d1
  initl = a
  RETURN
END FUNCTION eval3

end module meval

PROGRAM testentry
  use assertmod
  use meval
  IMPLICIT NONE
  REAL :: a = 1., b = 2., c = 1., d = 2.
  REAL :: value
  REAL :: I,result
  logical :: precision_R4,test
  value = initl ( a, b, c, d )
  I = 2.
  value = eval3 (i, result )
  test = precision_R4(result, 25.)
  call assert(test,'The result is not correct',9)

END PROGRAM testentry
