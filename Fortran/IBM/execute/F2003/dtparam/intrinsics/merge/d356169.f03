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
!* 1. DEFECT 356169
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

program d356169
   use m
   implicit none

   type(B) :: b1
   b1=B(type1=A(4)(ca="123"))
   if(b1%type1%l1 /= 4)                             error stop 10_4
   if(b1%type1%ca /= "123")                         error stop 11_4

end program

