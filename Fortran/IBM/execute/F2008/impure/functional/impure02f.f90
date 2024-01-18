!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : impure02f
!*
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!*  checks if an IMPURE elemental procedure works with INTENT(in) type dummy argument
!*  Also, checks the functionality of IMPURE with a 2D array as a dummy argument.
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM main
  IMPLICIT NONE

  INTEGER, PARAMETER :: n = 3000
  INTEGER, PARAMETER :: m = 3000
  INTEGER :: i,j
  INTEGER :: a(n,m), b(n,m)
  do i = 1,3000,1
  do j = 1,3000,1
  a(i,j) = i+j
  end do
  end do

  b(2:2000, 3:10) = final(a(2:2000, 3:10))


 CONTAINS

IMPURE ELEMENTAL FUNCTION final (a)
    INTEGER, INTENT(IN) :: a
     INTEGER :: final
     PRINT *, "Element inside impure is",a
    final = a+1
  END FUNCTION final

END PROGRAM main
