!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : impure01f
!*
!*  PROGRAMMER                 : Tapti Vaid
!*  DATE                       : 2012-04-16
!*  
!*  DESCRIPTION
!*
!*  -Checks if an IMPURE elemental procedure is called in the exact array element order. 
!*  -Checks if it works when variables are defined in the specification part
!*   of the IMPURE elemental procedure.
!*  -Uses a print statement inside the IMPURE elemental function.
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



PROGRAM main
  IMPLICIT NONE

  INTEGER, PARAMETER :: n = 3000
  INTEGER :: i
  INTEGER :: a(n), b(n)
  a = (/ (i, i = 1, n) /)
  

b(1:2500:5) = final(a(1:2500:5))
  
  
  CONTAINS

  IMPURE ELEMENTAL FUNCTION final (a)
    INTEGER :: a
    INTEGER :: final
    INTEGER :: sum
    sum = 2
    final = sum+a
    print*, "Element inside impure is",a
    print*, "output=",final 
  END FUNCTION final

END PROGRAM main
