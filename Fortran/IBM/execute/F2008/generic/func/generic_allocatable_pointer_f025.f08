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

      subroutine ptr_sub(w,x,y,z)
      implicit none
         real    :: w
         real    :: y
         integer :: x
         integer, pointer :: z

         x = (x+z)/(w+y)
         print *, "   inside ptr_sub()  w=", w," x=",x," y=",y," z=",z
      end subroutine

      subroutine alloc_sub(x,w,z,y )
      implicit none
         real :: x
         real :: y
         integer, allocatable :: w
         integer :: z

         x = x+y-w+z
         print *, "   inside alloc_sub()  x=",x," w=", w," z=",z," y=",y
      end subroutine

end module

program generic_allocatable_pointer_f025
use f2008_generic
implicit none
   type(Base)            :: base_res
   real                  :: r_res /-1.0/
   integer               :: i_res /-1/
   integer, target       :: i_tgt /444/
   integer, allocatable  :: i_alloc
   integer, pointer      :: i_ptr

   allocate(i_alloc)
   i_alloc = 64
   i_ptr => i_tgt

   call base_res%alloc_ptr_sub(r_res,i_alloc,3,4.0)
   if (r_res /= -58.0) error stop 1
   call base_res%alloc_ptr_sub(y=4.0, z=base_res%i, w=i_alloc,x=r_res)
   if (r_res /= -119.0) error stop 2
   call base_res%alloc_ptr_sub(2.0,i_res,4.0,i_ptr)
   if (i_res /= 73) error stop 3
   call base_res%alloc_ptr_sub(x=i_res,y=8.0,z=i_ptr,w=0.0)
   if (i_res /= 64) error stop 4

   print *," .... end"

end program
