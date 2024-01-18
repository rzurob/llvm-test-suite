!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullData01.f   
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
!* 3. INITIALIZE DERIVED TYPE POINTER WITH DATA STATEMENT
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

program nullData01
   use m
   implicit none
   
   type(B(3)),pointer     :: b1
   type(A(:)),pointer     :: a1 

   type(B(3)),target      :: b2=B(3)(null())
   type(A(6)),target      :: a2=A(6)(c1="123")   

   Data b1 /null()/ 
   Data a1 /null()/

   if(associated(b1))                error stop 10_4
   if(associated(a1))                error stop 11_4

   b1=>b2
   if(.not. associated(b1))          error stop 12_4
   if(b1%l2 /= 3)                    error stop 13_4
   if(associated(b1%a1))             error stop 14_4

   a1=>a2
   b1%a1=>a1
   if(.not. associated(b1%a1))       error stop 15_4
   if(b1%a1%l1 /= 6)                 error stop 16_4
   if(b1%a1%c1 /= "123")             error stop 17_4  

   call check()

   contains

     subroutine check()

        type(B(5)),pointer     :: bb1
        type(A(:)),pointer     :: aa1

        type(B(5)),target      :: bb2=B(5)(null())
        type(A(10)),target     :: aa2=A(10)(c1="000111")

        Data bb1 /null()/
        Data aa1 /null()/

        if(associated(bb1))                error stop 18_4
        if(associated(aa1))                error stop 19_4

        bb1=>bb2
        if(.not. associated(bb1))          error stop 20_4
        if(bb1%l2 /= 5)                    error stop 21_4
        if(associated(bb1%a1))             error stop 22_4

        aa1=>aa2
        bb1%a1=>aa1
        if(.not. associated(bb1%a1))       error stop 23_4
        if(bb1%a1%l1 /= 10)                error stop 24_4
        if(bb1%a1%c1 /= "000111")          error stop 25_4

     end subroutine

end program
