!*  ===================================================================
!*
!*  DIAGNOSTIC TESTED          : Automatic object is defined as a derived
!*                               type which has type parameter key word.
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
call fun(10, 10)
contains
subroutine fun(arg1, arg2)
integer, intent(in) :: arg1, arg2
type(base(l=arg1,k=4, m=arg2)) :: c
if (c%l /= 10) error stop 1
end  subroutine
end

