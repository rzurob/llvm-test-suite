!*  ===================================================================
!*
!*  DIAGNOSTIC TESTED          : Function return is an array of derived type
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

type(Base(4, 2)), target  :: obj1(2)

obj1 = func1(2)

! verify the function result
if (any(obj1%avar .ne. 100)) stop 12
contains
function func1(nn)
integer nn
! function result is defined as 'Base' type with non-constant
! type parameter
type(Base(4, nn)) func1(nn)
func1%avar = 100
end function

end

