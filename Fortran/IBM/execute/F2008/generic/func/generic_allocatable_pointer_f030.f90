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
      integer :: i
   end type

   interface assignment(=)
      module procedure assgn_alloc
      module procedure assgn_ptr
   end interface

   contains

      subroutine assgn_alloc ( x, y )
      implicit none
         type(Base), intent(inout), allocatable :: x
         type(Base), intent(in) :: y

         x%i = y%i + 10
         print *, "   inside assgn_alloc()  x%i=",x%i, "  y%i=",y%i
      end subroutine

      subroutine assgn_ptr ( x, y )
      implicit none
         type(Base), intent(inout), pointer :: x
         type(Base), intent(in) :: y

         x%i = y%i - 100
         print *, "   inside assgn_ptr()  x%i=",x%i, "  y%i=",y%i
      end subroutine

end module

program generic_allocatable_pointer_f030
use f2008_generic
implicit none
   type(Base), allocatable, target :: Base_alloc_tgt
   type(Base), allocatable         :: Base_alloc
   type(Base), pointer             :: Base_ptr
   type(Base)                      :: Base_val, Base_res

   allocate(Base_alloc_tgt, source = Base(700) )
   allocate(Base_alloc, source = Base(4000) )
   Base_ptr => Base_alloc_tgt

   Base_val%i = 999
   Base_res = Base_val
   if (Base_res%i /= 999) error stop 1
   Base_alloc = Base_res
   if (Base_alloc%i /= 1009) error stop 2
   Base_ptr = Base_res
   if (Base_ptr%i /= 899) error stop 3

   print *," .... end"

end program
