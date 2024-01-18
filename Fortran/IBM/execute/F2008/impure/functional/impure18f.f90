!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : impure18f
!*
!*  PROGRAMMER                 : Tapti Vaid
!*  DATE                       : 2012-04-16
!*  
!*  DESCRIPTION
!*
!* checks if an IMPURE elemental procedures works fine with a scalar argument that is
!*   a  2-D array  
!* 
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



PROGRAM main
  IMPLICIT NONE

  INTEGER, PARAMETER :: n = 3000
  INTEGER, PARAMETER :: m = 3500
  INTEGER :: i,j, k = 0
  INTEGER :: a(n,m), b(n,m), f
  f = 0
  do i = 1,3000,1
  do j = 1,3500,1
  a(i,j) = k+1
  k = k+1
  end do
  end do

  
   
 do i = 1,50,5
 do j = 5,1000,3
 b(i,j) = final(a(i, j), f)
  PRINT *,b(i,j), "::::",f
 end do
 end do
   CONTAINS
  
 
  IMPURE ELEMENTAL FUNCTION final (a, f1)
      IMPLICIT NONE
      INTEGER, INTENT(inout) :: f1
      
      INTEGER, INTENT(INOUT) :: a
      INTEGER :: final
      INTEGER :: sum
      sum = 2
      PRINT *, "Element in the Impure function is",a
       f1 = func1(a,a)
      
      final = sum + f1 
  END FUNCTION final

IMPURE ELEMENTAL FUNCTION func1(z,y)
    INTEGER, INTENT(IN) :: z
    INTEGER, INTENT(INOUT) :: y
    INTEGER :: func1
      y = func2(y)
    print*, "element is: ", z
    func1 = z*5+y

  END FUNCTION func1

PURE ELEMENTAL FUNCTION func2(x)
      INTEGER, INTENT(IN) :: x
      INTEGER :: func2
      func2 = x+5
END  FUNCTION func2

END PROGRAM main
   



