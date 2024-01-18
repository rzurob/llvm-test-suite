!*  ===================================================================
!*
!*  FUNCTIONAL TESTED          : Pass object to type bind procedure
!*                               - Check type parameter of poly object
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

module amod
type dt_base(k, d)
  integer, kind :: k
  integer, len  :: d
  integer :: element(d)
  integer :: avar
  contains
  procedure :: prt_tp => base_print
end type

type, extends(dt_base) :: dt_child(m)
  integer, len :: m
  integer :: bvar
  contains
  procedure :: prt_tp => child_print
end type

contains
subroutine base_print(pa)
class(dt_base(4, *)) :: pa

print *, "base type"

if (pa%k /= 4) error stop 1
if (pa%d /= 10) error stop 2
end subroutine

subroutine child_print(pa)
class(dt_child(4, *, *)) :: pa

print *, "child type"

if (pa%k /= 4) error stop 4
if (pa%d /= 20) error stop 5
if (pa%m /= 2) error stop 6
end subroutine

end module


use amod
class(dt_base(4, :)), pointer :: aptr
type(dt_base(4, 10)), target :: tgt1
type(dt_child(4, 20, 2)), target :: tgt2

aptr => tgt1
call aptr%prt_tp()

aptr => tgt2
call aptr%prt_tp()

end

