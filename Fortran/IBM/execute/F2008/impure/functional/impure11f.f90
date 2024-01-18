!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!* an IMPURE elemental procedure calls an IMPURE elemental procedure within itself
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
PROGRAM main
  IMPLICIT NONE
  INTEGER, PARAMETER :: n = 3000
  INTEGER :: i
  INTEGER :: a(n), b(n), f(n)

  f = 0
  a = (/ (i, i = 1, n) /)
  b(1:2500:5) = final(a(1:2500:5), f(1:2500:5))

  CONTAINS
  IMPURE ELEMENTAL FUNCTION final (a, f1)
      INTEGER, INTENT(INOUT) :: f1
      INTEGER, INTENT(IN) :: a
      INTEGER :: final
      INTEGER :: sum

      sum = 2
      f1 = func1(a)
      final = sum + f1
  END FUNCTION final

  IMPURE ELEMENTAL FUNCTION func1(z)
    INTEGER, INTENT(IN) :: z
    INTEGER :: func1
    print*, "element is: ", z
    func1 = z*50
  END FUNCTION func1
END PROGRAM main




