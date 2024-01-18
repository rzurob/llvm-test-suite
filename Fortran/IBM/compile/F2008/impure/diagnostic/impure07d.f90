!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!* diagnostic TC to make sure IMPURE prefix is neccessary for an elemental procedure to be impure.
!* Without any prefix, an elemental procedure is PURE by default
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
b(i) = func2(a(i))
end do

  CONTAINS

 ELEMENTAL FUNCTION func2(x)
      REAL, INTENT(IN) :: x
      REAL :: func2
      func2 = (x+10)/8.7
      print*, x
END  FUNCTION func2


  END PROGRAM main
