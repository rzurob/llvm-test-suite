!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 15 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : INTRINSICS(MERGE)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.75
!* 2. INTRINSICS:MERGE(TSOURCE,FSOURCE,MASK)
!* 3. DEFECT 356173
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

program d356173
   use m
   implicit none

   type(A(4)),target :: a1=A(4)(ca="123")
   type(B),target :: b1=B(type1=null())
   character(:),allocatable :: c1
   b1%type1=a1
   c1=merge(b1%type1%ca,"xlf",.true.)
   if(c1 /= "123")                                       error stop 10_4

end program

