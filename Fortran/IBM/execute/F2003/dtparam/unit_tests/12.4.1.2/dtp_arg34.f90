!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  DIAGNOSTIC TESTED          : Function return is a scalar derived type
!*                               object whic has non-constant type parameters.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

type Base(k, d)
  integer, kind :: k
  integer, len  :: d
  integer :: element(d)
  integer :: avar
end type

type(Base(4, 2)), target :: obj1

obj1 = func1(2)

! verify the result
if (obj1%avar /= 100) stop 1

contains
function func1(nn)
integer nn
! define the function result using type 'Base' with non-constant
! type parameter 'nn'
type(Base(4, nn)) func1
func1%avar = 100
end function

end
