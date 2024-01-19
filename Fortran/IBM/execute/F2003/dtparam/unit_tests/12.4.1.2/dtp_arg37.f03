!*  ===================================================================
!*
!*  DIAGNOSTIC TESTED          : Function return is an allocatable to a derived
!*                               type object with non-constant type parameters
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

type(Base(4, 2)) :: rest

rest = func1(2)

! verify the function result
if (rest%avar /= 100) error stop 1
contains

function func1(nn)
integer nn
! function result is an allocatable and defined as 'Base' type with
! non-constant type parameter
type(Base(4, nn)), allocatable :: func1

allocate(func1)
func1%avar = 100
end function

end
