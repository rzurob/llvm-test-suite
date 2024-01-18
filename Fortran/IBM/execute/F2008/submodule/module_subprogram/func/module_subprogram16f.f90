!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : module_subprogram16f.f
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : December 20, 2012
!*  
!*  DESCRIPTION
!*  based on F2008/impure/functional/impure16f.f
!*
!*  Define an impure module subroutine, testing compatibility with the F2008
!*   IMPURE feature
!*  
!*  Secondary tests:
!*  - order in which IMPURE keyword is specified
!*  
!*  Verify that the results match the values of the original test case.
!* 
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



MODULE imp
IMPLICIT NONE

INTEGER, PARAMETER :: n = 3000
  INTEGER :: i
  INTEGER :: z(n), b(n), sum = 2.2
    
  INTERFACE

    MODULE IMPURE SUBROUTINE final (z)
      INTEGER :: z
    END SUBROUTINE final
  END INTERFACE

  CONTAINS

    SUBROUTINE  func1(z)
      INTEGER :: z
      call  final(z)
    END SUBROUTINE func1

END MODULE imp

SUBMODULE (imp) impsub
CONTAINS
  MODULE PROCEDURE final
    print* , "old value of sum is: ", sum
    sum = 2*z
    print*, "sum inside impure is: ", sum
    z  = sum+z
    print*,"array element is: ", z
  END

END SUBMODULE impsub


PROGRAM main

  USE imp
  IMPLICIT NONE

  z = (/ (i, i = 1, n) /)
  
do i = 1,2500,5
call  func1(z(i))
   
  end do

END PROGRAM main

