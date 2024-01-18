!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : impure17f
!*
!*  PROGRAMMER                 : Tapti Vaid
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!* checks if an IMPURE elemental procedures works fine with a scalar argument.
!*  
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



PROGRAM main
  IMPLICIT NONE

      INTEGER :: f,b,a
      f = 3
       
      a = 15673.23
    b = final(a, f)
    PRINT *,b, "::::",f
 
CONTAINS


  IMPURE ELEMENTAL FUNCTION final (a, f1)
       INTEGER, INTENT(inout) :: f1

      INTEGER :: a
      INTEGER :: final
      INTEGER :: sum
      sum = 2

       f1 = func1(a,a)
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
      func2 = (x+10)/8
END  FUNCTION func2

END PROGRAM main
