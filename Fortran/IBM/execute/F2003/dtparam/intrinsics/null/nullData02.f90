!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullData02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 25 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : NULL([MOLD]) 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.88 
!* 2. NULL([MOLD])
!* 3. DERIVED TYPE IS POINTER ARRAY,USE DATA STATMENT 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l1)
      integer,len   :: l1
      character(l1) :: c1 
   end type
   type B(l2)
      integer,len        :: l2
      type(A(:)),pointer :: a1=>null() 
   end type
end module

program nullData02
   use m
   implicit none
  
   integer :: i
 
   type(B(6)),pointer     :: b1(:)
   type(A(3)),pointer     :: a1(:)

   Data a1 /null()/
   Data b1 /null()/

   if(associated(a1))                       error stop 7_4
   if(associated(b1))                       error stop 8_4
 
   allocate(a1(2),source=[A(3)(c1="123"),A(3)("456")])

   allocate(b1(2),source=[B(6)(a1=a1(1)),B(6)(a1=a1(2))])

   if(.not. associated(b1))                 error stop 10_4
   if(.not. associated(b1(1)%a1))           error stop 11_4
   if(.not. associated(b1(2)%a1))           error stop 12_4
   if(.not. associated(a1))                 error stop 13_4

   if(b1%l2 /= 6)                           error stop 14_4
   if(b1(1)%a1%l1 /= 3)                     error stop 15_4
   if(b1(2)%a1%l1 /= 3)                     error stop 16_4
   if(b1(1)%a1%c1 /= "123")                 error stop 17_4
   if(b1(2)%a1%c1 /= "456")                 error stop 18_4

!   a1=>null()
!   b1=>null()

!   if(associated(a1))                       error stop 19_4
!   if(associated(b1))                       error stop 20_4
    
end program
