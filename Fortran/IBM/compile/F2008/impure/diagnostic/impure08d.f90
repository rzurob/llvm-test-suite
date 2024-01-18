!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!* Diagnostic Test Case to make sure an IMPURE elemental procedure cannot be
!* referenced in a PURE elemental procedure.
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



PROGRAM main
  IMPLICIT NONE

  INTEGER, PARAMETER :: n = 3000
  INTEGER :: i
  REAL :: a(n), b(n), f
  f = 0.0
  a = (/ (i, i = 1, n) /)

do i = 1,2500,5
b(i) = final(a(i), f)
   PRINT *,b(i), "::::",f
  end do

CONTAINS


  IMPURE ELEMENTAL FUNCTION final (a, f1)
      IMPLICIT NONE
      REAL, INTENT(inout) :: f1

      REAL, INTENT(in) :: a
      REAL :: final
      REAL :: sum
      sum = 2.0

       f1 = func1(a)
      final = sum + f1
  END FUNCTION final

IMPURE ELEMENTAL FUNCTION func1(z)
    REAL, INTENT(IN) :: z

    REAL :: func1

    print*, "element is: ", z
    func1 = z*50

  END FUNCTION func1

PURE ELEMENTAL FUNCTION func2(x)
      REAL, INTENT(IN) :: x
      REAL :: func2
      REAL :: y
      y = 0.0
      func2 = func1 (y)
      !func2 = (x+10)/8.7
END  FUNCTION func2

END PROGRAM main




