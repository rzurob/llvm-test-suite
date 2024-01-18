!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : impure01d
!*
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!*  checks if an IMPURE elemental procedure compiled on vesions lower than F2008
!*  generates a langlvl message
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

    IMPURE ELEMENTAL FUNCTION foo(a)
      REAL :: foo
      real, intent(in) :: a
      foo = a+1
      print *, a

    END FUNCTION foo


END PROGRAM main
