!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : impure06f
!*
!*  PROGRAMMER                 : Tapti Vaid
!*  DATE                       : 2012-04-16
!*  
!*  DESCRIPTION
!*
!*  checks if an IMPURE elemental procedure works with a REAL data type 
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



PROGRAM main
  IMPLICIT NONE

  INTEGER, PARAMETER :: n = 3000
  INTEGER :: i
  REAL :: a(n), b(n)
  a = (/ (i, i = 1, n) /)
  do i = 1,2500,5

   b(i) =  func1 (a(i))
   PRINT *,b(i)
  end do

CONTAINS

REAL FUNCTION func1(z)
    REAL :: z
    
    func1 = func2(z)
  END FUNCTION func1

   REAL  FUNCTION func2(y)
    REAL :: y
    
    func2 = final(y)
 END FUNCTION func2


  IMPURE ELEMENTAL FUNCTION final (a)
    REAL, INTENT(IN) :: a
    REAL :: final
    REAL :: sum
    PRINT* ,"a  = ",a   
    sum = 2
    final = sum+a
  END FUNCTION final
END PROGRAM main
