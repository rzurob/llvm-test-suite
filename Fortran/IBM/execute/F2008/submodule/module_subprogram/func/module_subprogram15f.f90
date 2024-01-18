!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : module_subprogram15f
!*
!*  DATE                       : December 20, 2012
!*
!*  DESCRIPTION
!*  based on F2008/impure/functional/impure15f.f
!*
!*  Define an impure module function, testing compatibility with the F2008
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
  INTEGER :: z(n), b(n), sum = 2

  INTERFACE
    IMPURE MODULE ELEMENTAL FUNCTION final (z)
      INTEGER, INTENT(IN) :: z
      INTEGER :: final
    END FUNCTION final
  END INTERFACE

  CONTAINS

  FUNCTION func1(z)
    INTEGER :: z
    INTEGER :: func1
    func1 = final(z)
  END FUNCTION func1


END MODULE imp

SUBMODULE (imp) impsub
CONTAINS
  MODULE PROCEDURE final
    print* , "old value of sum is: ", sum
    sum = 2*z
    print*, "sum inside impure is: ", sum
    final = sum+z
  END

END SUBMODULE impsub


PROGRAM main
  USE imp
  IMPLICIT NONE

  z = (/ (i, i = 1, n) /)

  do i = 1,2500,5
   b(i) = func1(z(i))
   PRINT *,b(i)
  end do

END PROGRAM main

