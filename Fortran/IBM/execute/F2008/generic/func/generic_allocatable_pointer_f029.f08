!*
!*  ===================================================================
!*
!*  TYPE                       : Functional test
!*  FEATURE                    : #917301 F2008: Generic resolution extensions
!*  RTC Master Story           : 17301: F2008: Generic resolution extensions (master story)
!*                               https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/17301
!*
!*  DATE                       : 29 June 2012
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                :
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

module f2008_generic
implicit none

   type Base
      integer :: i /-1/
      contains
         procedure, nopass :: ptr_sub
         procedure, nopass :: alloc_sub
         generic :: alloc_ptr_sub => ptr_sub, alloc_sub
   end type

   contains

      subroutine ptr_sub(x,y)
      implicit none
         type(Base) :: x
         class(*), intent(inout), pointer :: y

         select type (y)
            class is (base)
               x%i = y%i
               print *, "   inside ptr_sub()   x=",x%i," y=",y%i
            class default
               error stop 12
         end select
      end subroutine

      subroutine alloc_sub(x,y)
      implicit none
        type(Base) :: x
        class(*), intent(inout), allocatable :: y

         select type (y)
            class is (base)
               x%i = y%i
               print *, "   inside alloc_sub()   x=",x%i," y=",y%i
            class default
               error stop 12
         end select
      end subroutine

end module

program generic_allocatable_pointer_f029
use f2008_generic
implicit none
   type(Base)                      :: base_val
   type(Base),allocatable         :: base_res
   type(Base), allocatable, target :: base_alloc_tgt
   class(*), allocatable           :: base_alloc
   class(*), pointer               :: base_ptr

   allocate(base_res, source = base(100) )
   allocate(base_alloc_tgt, source = base(700) )
   allocate(base_alloc, source = base(4000) )
   base_ptr => base_alloc_tgt

   call base_val%alloc_ptr_sub(base_res, base_ptr)
   if (base_res%i /= 700) error stop 1
   call base_res%alloc_ptr_sub(base_res, base_alloc)
   if (base_res%i /= 4000) error stop 2
   call base_val%alloc_ptr_sub(y=base_ptr, x=base_res)
   if (base_res%i /= 700) error stop 3


   print *," .... end"
end program
