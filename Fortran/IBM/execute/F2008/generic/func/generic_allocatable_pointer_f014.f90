!#######################################################################
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2008/generic/func/generic_allocatable_pointer_f014.f
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

program generic_allocatable_pointer_f014
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
         implicit none

         interface alloc_ptr_sub
            subroutine i_ptr_sub(x,y)
               integer, pointer ::x
               integer y
            end subroutine
            subroutine i_alloc_sub(x,y)
               integer,allocatable ::x
               integer y
            end subroutine
         end interface

         interface alloc_ptr_func
            integer function i_alloc_func(x)
               integer,allocatable ::x
            end function
            integer function i_ptr_func(x)
               integer, pointer::x
            end function
         end interface

         integer               :: i_res
         integer, target       :: i_tgt /-44444/
         integer, allocatable  :: i_alloc
         integer, pointer      :: i_ptr

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
