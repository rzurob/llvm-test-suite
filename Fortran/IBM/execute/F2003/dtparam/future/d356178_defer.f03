!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 15 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. DEFECT 356178
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l1)
       integer, len  :: l1
       character(l1) :: ca
   end type
   type B
       type(A(:)),allocatable :: type1
   end type
end module

program d356178
   use m
   implicit none

   type(A(4)) :: a1=A(4)(ca="123")
   type(B)    :: b1=B(type1=null())

   b1%type1=a1
   associate(x=>b1%type1%ca)
      if(x /= "123")                         error stop 10_4
      if(x%len /= len(x) .or. x%len /= 4)    error stop 11_4
   end associate

end program
