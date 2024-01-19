!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!* checks if a combination of PURE and IMPURE elemental procedures works fine to
!  output the right result
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
      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: f1
      INTEGER, INTENT(INOUT) :: a
      INTEGER :: final
      INTEGER :: sum

      sum = 2
      f1 = func1(a, a)
      final = sum + f1
  END FUNCTION final

  IMPURE ELEMENTAL FUNCTION func1(z,y)
    INTEGER, INTENT(IN) :: z
    INTEGER, INTENT(INOUT) :: y
    INTEGER :: func1

    y = func2(y)
    print*, "element is: ", z
    func1 = z*50+y

  END FUNCTION func1

  PURE ELEMENTAL FUNCTION func2(x)
      INTEGER, INTENT(IN) :: x
      INTEGER :: func2
      func2 = (x+10)/2
  END  FUNCTION func2
END PROGRAM main
