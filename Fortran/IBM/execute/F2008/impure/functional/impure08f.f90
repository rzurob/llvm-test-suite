!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!*  checks if an IMPURE elemental procedure works with a variable declared with a SAVE attribute
!*  whose value is later on changed wihthin the function and printed
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



PROGRAM main
  IMPLICIT NONE

  INTEGER, PARAMETER :: n = 3000
  INTEGER :: i
  INTEGER :: a(n), b(n), finalsum
  a = (/ (i, i = 1, n) /)
  finalsum = 0
do i = 1,2500,5
b(i) = func1(a(i), finalsum)
   PRINT *,b(i)
  end do


  CONTAINS

   INTEGER FUNCTION func1(z, s)
   INTEGER :: z

    INTEGER :: s
    func1 = func2(z, s)
  END FUNCTION func1

   INTEGER  FUNCTION func2(y, s)
    INTEGER :: y
    INTEGER :: s

    func2 = final(y, s)
 END FUNCTION func2



  IMPURE ELEMENTAL FUNCTION final (a, sum)

    INTEGER, INTENT(IN) :: a
    INTEGER :: final
    INTEGER, INTENT(INOUT) :: sum
    INTEGER, save :: incrementsum = 2
    sum  = incrementsum+sum
    incrementsum = incrementsum + 1
    print*, "new increment=", incrementsum
    final = sum+a

  END FUNCTION final

END PROGRAM main
