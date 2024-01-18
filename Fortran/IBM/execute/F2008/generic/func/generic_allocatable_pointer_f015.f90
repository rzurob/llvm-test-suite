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
     integer               :: i_res
     integer, target       :: i_tgt /-44444/
     integer, allocatable  :: i_alloc
     integer, pointer      :: i_ptr

contains
      subroutine i_alloc_sub(x,y)
         implicit none
         integer,allocatable :: x
         integer y
         y = x
         print *, "   inside i_alloc_sub()  x=",x,"  y=",y
      end subroutine
      subroutine i_ptr_sub(x,y)
         implicit none
         integer, pointer, intent(inout) :: x
         integer y
         y = x
         print *, "   inside i_ptr_sub()  x=",x,"  y=",y
      end subroutine
      integer function i_alloc_func(x)
         implicit none
         integer,allocatable :: x
         print *, "   inside i_alloc_func()  x=",x
         i_alloc_func = x
      end function
      integer function i_ptr_func(x)
         implicit none
         integer, pointer, intent(inout) :: x
         print *, "   inside i_ptr_func()  x=",x
         i_ptr_func = x
      end function

end module

program generic_allocatable_pointer_f015
use f2008_generic
implicit none
    integer inmain /1000/

    call subintr_l1(inmain)
    print *, "   inside main()  inmain=",inmain
    if (inmain /= 606) error stop 66

contains

      subroutine subintr_l1(x)
      implicit none
         integer x

         x = x + 10
         print *, "   inside subintr_l1()  x=",x
         call subintr_l2(x)
      end subroutine

      subroutine subintr_l2(x)
      implicit none
         integer x

         x = x / 5
         print *, "   inside subintr_l2()  x=",x
         call subintr_l3(x)
      end subroutine

      subroutine subintr_l3(x)
!      use f2008_generic
      implicit none

         interface alloc_ptr_sub
            MODULE PROCEDURE i_alloc_sub, i_ptr_sub
         end interface

         interface alloc_ptr_func
            MODULE PROCEDURE i_alloc_func, i_ptr_func
         end interface

         integer x

         x = x * 3
         print *, "   inside subintr_l3()  x=",x

         allocate(i_alloc)
         i_alloc = 77777
         i_ptr => i_tgt

         call alloc_ptr_sub(i_alloc, i_res)
         if (i_res /= 77777) error stop 1
         call alloc_ptr_sub(i_ptr, i_res)
         if (i_res /= -44444) error stop 2
         call alloc_ptr_sub(y=i_res, x=i_alloc)
         if (i_res /= 77777) error stop 3
         call alloc_ptr_sub(y=i_res, x=i_ptr)
         if (i_res /= -44444) error stop 4

         i_res = alloc_ptr_func(i_alloc)
         if (i_res /= 77777) error stop 5
         i_res = alloc_ptr_func(i_ptr)
         if (i_res /= -44444) error stop 6

      end subroutine
end program
