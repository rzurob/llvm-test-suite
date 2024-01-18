!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : impure14f
!*
!*  PROGRAMMER                 : Tapti Vaid
!*  DATE                       : 2012-04-16
!*  
!*  DESCRIPTION
!*
!* Uses IMPURE elemental procedure in a module. The global values of the module 
!* are also being changed in the IMPURE elemental procedure.
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



MODULE imp

IMPLICIT NONE
INTEGER, PARAMETER :: n = 3000
  INTEGER :: i
  INTEGER :: z(n), b(n), sum = 2
    
  CONTAINS

    FUNCTION func1(z)
    INTEGER :: z
    INTEGER :: func1
    func1 = final(z)
  END FUNCTION func1


  IMPURE ELEMENTAL FUNCTION final (z)
    INTEGER :: z
    INTEGER :: final
   
      print* , "old value of sum is: ", sum
    sum = 2*z
    print*, "sum inside impure is: ", sum
    final = sum+z
  END FUNCTION final

END MODULE imp


PROGRAM main
  USE imp
  IMPLICIT NONE



  z = (/ (i, i = 1, n) /)
  
do i = 1,2500,5
b(i) = func1(z(i))
   PRINT *,b(i)
  end do

END PROGRAM main

