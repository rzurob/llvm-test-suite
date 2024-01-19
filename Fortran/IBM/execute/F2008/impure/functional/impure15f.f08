!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!* Uses IMPURE elemental procedure as an INTERFACE.
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



  IMPURE ELEMENTAL FUNCTION final (z)
    IMPLICIT  NONE
    INTEGER, INTENT(IN) :: z

    INTEGER :: final
    INTEGER :: sum = 0.0
    sum = 2*z

    final = sum+z
  END FUNCTION final




PROGRAM main

  IMPLICIT NONE

INTERFACE imp
    IMPURE ELEMENTAL function final(z)
      implicit none
      INTEGER, INTENT(IN) :: z
      INTEGER :: final
    END FUNCTION
end INTERFACE

  INTEGER, PARAMETER :: n = 3000
  INTEGER :: i
  INTEGER :: z(n), b(n)
  z = (/ (i, i = 1, n) /)

do i = 1,2500,5
b(i) = final(z(i))
   PRINT *,b(i)
  end do

END PROGRAM main

