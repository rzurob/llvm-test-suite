!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullMoldIsRes01.f
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
!* 3. MOLD IS FUNCTION RESULT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1)
     integer,kind :: k1
     integer(k1)  :: i1
   end type
   type,extends(base) :: child(k2)
     integer,kind :: k2
     integer(k2)  :: i2
   end type
   contains

     function func1(child2)
        class(base(2)),pointer :: child2
        class(base(2)),pointer :: func1
        if(associated(child2)) deallocate(child2)
        allocate(child2,source=child(2,2)(i1=1,i2=2))
        func1=>child2
     end function
end module

program nullMoldIsRes01
   use m
   implicit none

   class(base(2)),pointer     :: b1=>null()
   class(base(2)),pointer     :: b2=>null()

   if(associated(b1))                              error stop 11_4

   b1=>func1(b2)

   if(.not. associated(b1))                        error stop 12_4

   select type(b1)
      type is(child(2,2))
         if(b1%k1 /= 2)                            error stop 13_4
         if(b1%k2 /= 2)                            error stop 14_4
         if(b1%i1 /= 1)                            error stop 15_4
         if(b1%i2 /= 2)                            error stop 16_4
      class default
         error stop 100_4
   end select

   b1=>null(func1(b2))

   if(associated(b1))                              error stop 17_4

   b1=>func1(b2)

   if(.not. associated(b1))                        error stop 18_4


end program

