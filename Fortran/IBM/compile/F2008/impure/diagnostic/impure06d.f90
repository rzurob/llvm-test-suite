!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : impure06d
!*
!*  PROGRAMMER                 : Tapti Vaid
!*  DATE                       : 2012-04-16
!*  
!*  DESCRIPTION
!*
!* diagnostic TC to make sure IMPURE elemental feature does not work with FORALL
!*  
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



PROGRAM main
  IMPLICIT NONE

  INTEGER, PARAMETER :: n = 3000
  INTEGER :: i
  REAL :: a(n), b(n)
  a = (/ (i, i = 1, n) /)
  
forall (i=500:2500) 
b(i) = func2(a(i))
   
  end forall

forall (i=500:2500)
b(i) = func1(a(i))
   
  end forall

  CONTAINS

IMPURE ELEMENTAL FUNCTION func1(z)
    REAL, INTENT(IN) :: z
    REAL :: func1
      print*, "element is: ", z
    func1 = z*50

  END FUNCTION func1

PURE ELEMENTAL FUNCTION func2(x)
      REAL, INTENT(IN) :: x
      REAL :: func2
      func2 = (x+10)/8.7
END  FUNCTION func2


  END PROGRAM main
