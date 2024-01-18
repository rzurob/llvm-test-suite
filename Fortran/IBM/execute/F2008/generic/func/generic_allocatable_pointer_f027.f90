!######################################################################i
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2008/generic/func/generic_allocatable_pointer_f027.f
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
         procedure, pass(dt_var) :: ptr_sub
         generic :: alloc_ptr_sub => ptr_sub
   end type

   type, extends(Base) :: Child
      contains
         procedure, pass(dt_var) :: alloc_sub
         generic :: alloc_ptr_sub => alloc_sub
   end type

   contains

      subroutine ptr_sub(y,x,dt_var)
      implicit none
         class(Base) :: y
         type(Base), pointer :: x
         class(Base), intent(inout) :: dt_var

!         print *, "1.    inside ptr_sub()  y=", y%i," x=",x%i," dt_var%i=",dt_var%i
         x%i = 3*y%i + dt_var%i + x%i
         print *, "   inside ptr_sub()  y=", y%i," x=",x%i," dt_var%i=",dt_var%i
      end subroutine

      subroutine alloc_sub(x,y,dt_var )
      implicit none
        type(Base) , allocatable :: x
        class(Base) :: y
        class(Child), intent(inout) :: dt_var

!         print *, "1.   inside alloc_sub()  x=",x%i," y=",y%i," dt_var%i=",dt_var%i
         x%i = (x%i+ y%i - dt_var%i) / 5
         print *, "   inside alloc_sub()  x=",x%i," y=",y%i," dt_var%i=",dt_var%i
      end subroutine

end module

program generic_allocatable_pointer_f027
use f2008_generic
implicit none
   type(Base)                       :: base_res
   type(Child)                      :: child_res = Child(1000)
   type(Child)                      :: child_res2 = Child(-200)

   type(Base), allocatable, target :: base_alloc_tgt
   type(Base), allocatable         :: base_alloc
   type(Base), pointer             :: base_ptr


   allocate(base_alloc_tgt, source = base(700) )
   allocate(base_alloc, source = base(4000) )
   base_ptr => base_alloc_tgt


   call child_res%alloc_ptr_sub(base_alloc,child_res2)
   if (base_alloc%i /= 560) error stop 1
   call child_res%alloc_ptr_sub(y=child_res2, x=base_alloc)
   if (base_alloc%i /= -128) error stop 2
   call child_res%alloc_ptr_sub(child_res2,base_ptr)
   if (base_ptr%i /= 1100) error stop 3
   call child_res%alloc_ptr_sub(base_alloc_tgt,base_ptr)
   if (base_ptr%i /= 240) error stop 4

   print *," .... end"

end program

