!*  ===================================================================
!*
!*  DIAGNOSTIC TESTED          : Mix of automatic object array and dummy
!*                               argument with unknown type parameters.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

type dt_base(k, d)
  integer, kind :: k
  integer, len :: d
  integer :: element(d, d+2)
  integer :: avar
end type

integer num
type(dt_base(k=4, d=2)) :: obj1

obj1%avar = 100
obj1%element = 4
num = 2

call sub1(obj1, num)

contains
subroutine sub1(pa, len_tp)
integer len_tp
type(dt_base(4, *)) :: pa
type(dt_base(4, len_tp)) :: pb
integer rest(ubound(pa%element, 1), ubound(pa%element, 2))

print *, shape(rest)

rest = pa%element
pb = pa

print *, rest
print *, pa%avar

end subroutine

end
