!*  ============================================================================
!*
!*  TEST CASE NAME             : impure03f
!*
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!*  checks if an IMPURE elemental procedure works with INTENT(out) type dummy argument
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM main
  IMPLICIT NONE

  INTEGER, PARAMETER :: n = 3000
  INTEGER :: i
  INTEGER :: s = 0
  INTEGER :: a(n), b(n)
  a = (/ (i, i = 1, n) /)
  do i = 2,2000,2

   b(i) = func1 (a(i),s)
   PRINT *,b(i)
  end do

 CONTAINS

   INTEGER FUNCTION func1(z,s)
   INTEGER :: z
   INTEGER :: s
    func1 = func2(z,s)
  END FUNCTION func1

   INTEGER  FUNCTION func2(y,s)
   INTEGER :: y
   INTEGER :: s
    func2 = final(y,s)
 END FUNCTION func2

  IMPURE ELEMENTAL FUNCTION final (a,s)
    INTEGER, INTENT(IN) :: a
    INTEGER, INTENT(out) :: s
    INTEGER :: final
    INTEGER :: sum
    sum = 2
    s = a*2
    final  = sum+a
  END FUNCTION final
END PROGRAM main
