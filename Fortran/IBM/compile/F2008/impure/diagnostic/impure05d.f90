!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : impure05d
!*
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!*  checks if an error is generated when there is more than one occurrence of the
!*  keyword IMPURE in the definition of an elemental procedure
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

    IMPURE IMPURE ELEMENTAL FUNCTION foo(a)
      REAL :: foo
      real, intent(in) :: a
      foo = a+1


    END FUNCTION foo


END PROGRAM main
