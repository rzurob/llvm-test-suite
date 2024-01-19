!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!*  checks if an IMPURE elemental procedure which is also RECURSIVE, generates an error
!*  message
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

    RECURSIVE IMPURE ELEMENTAL FUNCTION foo(a)
      REAL :: foo
      real, intent(in) :: a
      foo = foo(a)+1
      print *, a

    END FUNCTION foo


END PROGRAM main
