!*  ===================================================================
!*
!*  DIAGNOSTIC TESTED          : Mix 'auto' and explicit type parameters
!*                               in declaration.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

type base(k, l, m)
   integer, kind :: k
   integer, len  :: l, m
   integer arra(l)
end type
call fun(8, 10)
contains
subroutine fun(arg1, arg2)
integer, intent(in) :: arg1, arg2
type(base(k=4, l=arg1, m=2)) :: c
if (c%l /= 8) error stop 2
end  subroutine
end
