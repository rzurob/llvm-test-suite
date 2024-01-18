!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : impure16f
!*
!*  PROGRAMMER                 : Tapti Vaid
!*  DATE                       : 2012-04-16
!*  
!*  DESCRIPTION
!*
!* Uses IMPURE elemental procedure in a module as a subroutine.  
!* 
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



MODULE imp
IMPLICIT NONE

INTEGER, PARAMETER :: n = 3000
  INTEGER :: i
  INTEGER :: z(n), b(n), sum = 2.2
    
  CONTAINS

     SUBROUTINE  func1(z)
     INTEGER :: z
     call  final(z)
     END SUBROUTINE func1


  IMPURE SUBROUTINE final (z)
    INTEGER :: z
    print* , "old value of sum is: ", sum
    sum = 2*z
    print*, "sum inside impure is: ", sum
    z  = sum+z
    print*,"array element is: ", z
  END SUBROUTINE final

END MODULE imp


PROGRAM main

  USE imp
  IMPLICIT NONE

  z = (/ (i, i = 1, n) /)
  
do i = 1,2500,5
call  func1(z(i))
   
  end do

END PROGRAM main

