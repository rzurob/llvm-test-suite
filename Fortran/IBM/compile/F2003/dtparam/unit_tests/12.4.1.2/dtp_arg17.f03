!*  ===================================================================
!*
!*  FUNCTIONAL TESTED          : Dummy argument has expression of type
!*                               parameter.
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
  integer, len  :: d
  integer :: element(d, 2+d) ! - If changed to 'd+2', the ICE is gone (321751).
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
type(dt_base(4, len_tp)) :: pa
end subroutine

end

