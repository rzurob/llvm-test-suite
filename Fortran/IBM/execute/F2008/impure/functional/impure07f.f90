!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : impure07f
!*
!*  PROGRAMMER                 : Tapti Vaid
!*  DATE                       : 2012-04-16
!*  
!*  DESCRIPTION
!*
!*  checks if an IMPURE elemental procedure works with a STOP statement 
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



PROGRAM main
  IMPLICIT NONE

  INTEGER, PARAMETER :: n = 3000
  INTEGER :: i
  INTEGER :: a(n), b(n)
  a = (/ (i, i = 1, n) /)
  do i = 1,2500,5

   b(i) = func1 (a(i))
   PRINT *,b(i)
   end do

CONTAINS

INTEGER FUNCTION func1(z)
    INTEGER :: z
    
    func1 = func2(z)
  END FUNCTION func1

   INTEGER  FUNCTION func2(y)
   INTEGER :: y
    
    func2 = final(y)
 END FUNCTION func2

  IMPURE ELEMENTAL FUNCTION final (a)
    INTEGER, INTENT(INOUT) :: a
    INTEGER :: final
    INTEGER :: sum
    PRINT* ,"a  = ",a   
    sum = 2
    STOP
    final = sum+a
  END FUNCTION final
END PROGRAM main
