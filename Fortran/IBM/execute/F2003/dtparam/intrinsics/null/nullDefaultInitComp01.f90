!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullDefaultInitComp01.f
!*
!*  DATE                       : Sept. 26 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : NULL([MOLD])
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.88
!* 2. NULL([MOLD])
!* 3. DEFAULT INITIALIZATION FOR A DERIVED TYPE COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(ka)
       integer,kind :: ka
       integer,allocatable :: i1
       integer,pointer     :: i2=>null()
   end type
   type B(kb)
       integer,kind :: kb
       type(A(kb)),allocatable :: a1
       type(A(kb)),pointer     :: a2=>null()
   end type
end module

program nullDefaultInitComp01
   use m
   implicit none

   type(B(4)) :: btype

   type(A(4)),target :: atype

   integer,target :: i=2

   if(btype%kb /= 4)                             error stop 10_4
   if(allocated(btype%a1))                       error stop 11_4
   if(associated(btype%a2))                      error stop 12_4

   btype%a1=atype
   btype%a2=>atype

   if(.not. allocated(btype%a1))                 error stop 13_4
   if(.not. associated(btype%a2))                error stop 14_4
   if(btype%a1%ka /= 4)                          error stop 15_4
   if(btype%a2%ka /= 4)                          error stop 16_4

   if(allocated(btype%a1%i1))                    error stop 17_4
   if(associated(btype%a1%i2))                   error stop 18_4
   if(allocated(btype%a2%i1))                    error stop 19_4
   if(associated(btype%a2%i2))                   error stop 20_4

   btype%a1%i1=i
   btype%a1%i2=>i
   btype%a2%i1=i
   btype%a2%i2=>i

   if(.not. allocated(btype%a1%i1))              error stop 21_4
   if(.not. associated(btype%a1%i2))             error stop 22_4
   if(.not. allocated(btype%a2%i1))              error stop 23_4
   if(.not. associated(btype%a2%i2))             error stop 24_4
   if(btype%a1%i1 /= 2)                          error stop 25_4
   if(btype%a1%i2 /= 2)                          error stop 26_4
   if(btype%a2%i1 /= 2)                          error stop 27_4
   if(btype%a2%i2 /= 2)                          error stop 28_4



end program
