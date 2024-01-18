!######################################################################i
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2008/generic/func/generic_allocatable_pointer_f028.f
!*  TYPE                       : Functional test
!*  FEATURE                    : #917301 F2008: Generic resolution extensions
!*  RTC Master Story           : 17301: F2008: Generic resolution extensions (master story)
!*                               https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/17301
!*
!*  PROGRAMMER                 : Grigor Nikolov
!*  DATE                       : 29 June 2012
!*  ORIGIN                     : XLF Test -  IBM Toronto Lab
!*
!*  DRIVER STANZA              : xlf2008
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
         procedure, pass :: ptr_sub
         generic :: alloc_ptr_sub => ptr_sub
         procedure, pass :: alloc_sub
         generic :: alloc_ptr_sub => alloc_sub
   end type

   type, extends(Base) :: Child
      integer :: j /3/
   end type

   contains

      subroutine ptr_sub(x,y)
      implicit none
         class(Base) :: x
         class(Base), pointer :: y

         y%i = 3*y%i + x%i
         print *, "   inside ptr_sub()   x=",x%i," y=",y%i
      end subroutine

      subroutine alloc_sub(x,y)
      implicit none
        class(Base) :: x
        class(Base), allocatable :: y

         y%i = (x%i+ y%i) / 5
         print *, "   inside alloc_sub()  x=",x%i," y=",y%i
      end subroutine

end module

program generic_allocatable_pointer_f028
use f2008_generic
implicit none
   type(Base)                       :: base_res = Base(100)
   type(Base), allocatable, target  :: base_alloc_tgt
   class(Base), allocatable         :: base_alloc
   class(Base), pointer             :: base_ptr

   type(Child)                      :: child_res = Child(1000)
   type(Child), allocatable, target :: child_alloc_tgt
   class(Child), allocatable        :: child_alloc
   class(Child), pointer            :: child_ptr


   allocate(base_alloc_tgt, source = base(700) )
   allocate(base_alloc, source = base(4000) )
   base_ptr => base_alloc_tgt

   call base_res%alloc_ptr_sub(base_ptr)
   if (base_ptr%i /= 2200) error stop 1
   call base_res%alloc_ptr_sub(base_alloc)
   if (base_alloc%i /= 820) error stop 2


   allocate(child_alloc_tgt, source = child(-500) )
   deallocate(base_alloc)
   allocate(base_alloc, source = child(-10000) )
   base_ptr => child_alloc_tgt

   call child_res%alloc_ptr_sub(base_ptr)
   if (base_ptr%i /= -500) error stop 3
   call child_res%alloc_ptr_sub(base_alloc)
   if (base_alloc%i /= -1800) error stop 4

   print *," .... end"
end program
