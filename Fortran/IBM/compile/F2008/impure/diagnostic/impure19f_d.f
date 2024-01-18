!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : impure19f
!*
!*  PROGRAMMER                 : Tapti Vaid
!*  DATE                       : 2012-04-16
!*  
!*  DESCRIPTION
!*
!* Uses an IMPURE elemental procedure with PURE and IMPURE internal elemental 
!* procedures 
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



  IMPURE ELEMENTAL FUNCTION final (z)
    IMPLICIT  NONE
    INTEGER :: z
    INTEGER :: final
    INTEGER :: sum = 0.0 
    sum = f1(z)
    print*, "Elements inside impure are", z, sum
    final = sum+z
        

    CONTAINS
    IMPURE ELEMENTAL FUNCTION f1(y)
    IMPLICIT  NONE
    INTEGER :: y
    INTEGER :: f1
    f1 = f2(y)
    END FUNCTION f1
    
   PURE ELEMENTAL FUNCTION f2(x)
   IMPLICIT  NONE
    INTEGER, INTENT(in) :: x
    INTEGER :: f2
    f2 = x+4
    END FUNCTION f2

  END FUNCTION final




PROGRAM main
  
  IMPLICIT NONE

INTERFACE imp
    IMPURE ELEMENTAL function final(z)
      implicit none
      INTEGER :: z
      INTEGER :: final
    END FUNCTION
end INTERFACE

  INTEGER, PARAMETER :: n = 3000
  INTEGER :: i
  INTEGER :: z(n), b(n)
  z = (/ (i, i = 1, n) /)
  

b(1:2500:5) = final(z(1:2500:5))
 
 

END PROGRAM main

