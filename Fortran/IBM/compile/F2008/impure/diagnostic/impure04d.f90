!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : impure04d
!*
!*  PROGRAMMER                 : Tapti Vaid
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!*  checks if an error message is generated when an elemental procedure is defined
!*  both as PURE and IMPURE
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



PROGRAM main
  IMPLICIT NONE

    integer :: ivar
    INTEGER, PARAMETER :: n = 5
      REAL :: a(n), b(n)
      INTEGER :: i
      a = (/ (i, i = 1, n) /)
      b = foo(a)

  contains

    PURE IMPURE ELEMENTAL FUNCTION foo(a)
      REAL :: foo
      real, intent(in) :: a
      foo = a+1
     

    END FUNCTION foo


END PROGRAM main
