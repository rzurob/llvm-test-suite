!*  ===================================================================
!*
!*  DIAGNOSTIC TESTED          : Automatic object is defined as a derived
!*                               type which has an allocatable component
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

type base(k, l)
   integer, kind :: k
   integer, len  :: l
   integer, allocatable ::  arr2
end type

contains

subroutine sub(arg)
   integer, intent(in) :: arg
   type(base(4, arg)) :: c
end  subroutine
end
