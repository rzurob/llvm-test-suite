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
         procedure, nopass :: t_ptr_sub
         generic :: alloc_ptr_sub => t_ptr_sub
   end type

   type, extends(Base) :: Child
      contains
         procedure, nopass :: t_alloc_sub
         generic :: alloc_ptr_sub => t_alloc_sub
   end type

   contains

      subroutine t_ptr_sub(x, y, z)
      implicit none
         type(Base) :: x
         type(Child), pointer :: y
         class(Base), optional :: z

         x%i = y%i
         print *, "   inside t_ptr_sub()  x=", x, "  present(z)=", present(z)
      end subroutine

      subroutine t_alloc_sub(x, y, z )
      implicit none
         type(Base) :: x
         type(Child),allocatable :: y
         class(Base), optional, intent(in) :: z

         x%i = y%i
         print *, "   inside t_alloc_sub()  x=", x, "  present(z)=", present(z)
      end subroutine

end module

program generic_allocatable_pointer_f022
use f2008_generic
implicit none
   type(Base)                       :: base_res
   type(Child), allocatable, target :: child_alloc_tgt
   type(Child), allocatable         :: child_alloc
   type(Child), pointer             :: child_ptr

   allocate(child_alloc_tgt, source = child(77777) )
   allocate(child_alloc, source = child(4444) )
   child_ptr => child_alloc_tgt

   call base_res%alloc_ptr_sub(base_res, child_ptr)
   if (base_res%i /= 77777) error stop 1
   call child_ptr%alloc_ptr_sub(base_res, child_alloc)
   if (base_res%i /= 4444)  error stop 2
   call child_alloc%alloc_ptr_sub(base_res, child_ptr)
   if (base_res%i /= 77777) error stop 3
   call child_ptr%alloc_ptr_sub(y=child_alloc, x=base_res)
   if (base_res%i /= 4444)  error stop 4
   call child_alloc%alloc_ptr_sub(y=child_ptr, x=base_res)
   if (base_res%i /= 77777) error stop 5

   base_res%i = 0
   call child_ptr%alloc_ptr_sub(base_res, child_alloc, child_alloc_tgt)
   if (base_res%i /= 4444)  error stop 6
   child_alloc%i = 101010
   base_res%i = 0
   call child_alloc%alloc_ptr_sub(base_res, z=child_alloc_tgt, y=child_alloc)
   if (base_res%i /= 101010)  error stop 7

   call child_ptr%alloc_ptr_sub(base_res, child_ptr, child_alloc_tgt)
   if (base_res%i /= 77777)  error stop 8
   call child_alloc%alloc_ptr_sub(z=child_alloc, y=child_ptr, x=base_res)
   if (base_res%i /= 77777)  error stop 9

   print *," .... end"

end program
