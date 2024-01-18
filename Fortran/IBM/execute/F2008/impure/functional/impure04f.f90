!*  ============================================================================
!*
!*  TEST CASE NAME             : impure04f
!*
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!*  checks if an IMPURE elemental procedure works with INTENT(inout) type dummy argument
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM main
  IMPLICIT NONE

  INTEGER, PARAMETER :: n = 3000
  INTEGER :: i
  INTEGER :: a(n), b(n), s(n)
  a = (/ (i, i = 1, n) /)
  do i = 2,2000,2
   s = 5

   b(i) = func1 (a(i), s(i))
   PRINT *,b(i)
  end do


CONTAINS

  INTEGER FUNCTION func1(z, k)
    INTEGER :: z
    INTEGER :: k

    func1 = func2(z,k)
  END FUNCTION func1

   INTEGER  FUNCTION func2(y, l)
    INTEGER :: y
    INTEGER :: l

    func2 = final(y,l)
 END FUNCTION func2


  IMPURE ELEMENTAL FUNCTION final (a, s)
    INTEGER, INTENT(IN) :: a
    INTEGER, INTENT(INOUT) :: s
    INTEGER :: final
    s = a+s
    final = s
  END FUNCTION final
END PROGRAM main
