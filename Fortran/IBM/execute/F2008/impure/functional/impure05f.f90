!*  ============================================================================
!*
!*  TEST CASE NAME             : impure05f
!*
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!*  checks if an IMPURE function (non elemental IMPURE procedure) works
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM main
  IMPLICIT NONE

  INTEGER, PARAMETER :: n = 3000
  INTEGER :: i
  INTEGER :: a(n), b(n), s(n)
  a = (/ (i, i = 1, n) /)
  do i = 2,2000,2

   b(i) = func1 (a(i))
   PRINT *,b(i)
  end do

CONTAINS

  INTEGER  FUNCTION func1(z)
    INTEGER :: z

    func1 = func2(z)
  END FUNCTION func1

   INTEGER  FUNCTION func2(y)
   INTEGER:: y

    func2 = final(y)
 END FUNCTION func2


  IMPURE  FUNCTION final (a)
    INTEGER  :: a
    INTEGER :: final
    final = a
  END FUNCTION final
END PROGRAM main
