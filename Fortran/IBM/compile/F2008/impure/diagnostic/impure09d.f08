!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!* Diagnostic Test Case that makes sure all internal subprograms of a pure subprogram must be pure
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



  PURE ELEMENTAL FUNCTION final (z)
    IMPLICIT  NONE
    REAL, INTENT(in) :: z

    REAL :: final

    REAL :: notallowed
    final = 5+z

   CONTAINS

   IMPURE ELEMENTAL FUNCTION notallowed(y)
   IMPLICIT NONE
   REAL :: notallowed
    REAL :: y
    y = y+1
    END FUNCTION notallowed
  END FUNCTION final




PROGRAM main

  IMPLICIT NONE

INTERFACE imp
    PURE ELEMENTAL function final(z)
      implicit none
      REAL, INTENT(in) :: z
      REAL :: final
    END FUNCTION
end INTERFACE

  INTEGER, PARAMETER :: n = 3000
  INTEGER :: i
  REAL :: z(n), b(n)
  z = (/ (i, i = 1, n) /)

do i = 1,2500,5
b(i) = final(z(i))
   PRINT *,b(i)
  end do

END PROGRAM main
