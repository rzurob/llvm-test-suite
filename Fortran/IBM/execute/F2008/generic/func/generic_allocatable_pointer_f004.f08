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

program generic_allocatable_pointer_f004
implicit none
   interface alloc_ptr_sub
      subroutine i_ptr_sub(x,y)
         integer, pointer :: x
         integer :: y
      end subroutine
      subroutine i_alloc_sub(x,y)
         integer,allocatable :: x
         integer :: y
      end subroutine
   end interface

   interface alloc_ptr_func
      integer function i_alloc_func(x)
         integer,allocatable :: x
      end function
      integer function i_ptr_func(x)
         integer, pointer :: x
      end function
   end interface

     integer               :: i_res
     integer, target       :: i_tgt
     integer, allocatable  :: i_alloc
     integer, target, allocatable  :: i_alloc_tgt
     integer, pointer      :: i_ptr, i_ptr_for_alloc

     allocate(i_alloc_tgt)
     i_alloc_tgt = 44444
     i_ptr => i_alloc_tgt

     i_tgt = 77777
     i_ptr_for_alloc => i_tgt
     allocate(i_alloc, source=i_ptr_for_alloc)

     call alloc_ptr_sub(i_alloc, i_res)
     if (i_res /= 77777) error stop 1
     call alloc_ptr_sub(i_ptr, i_res)
     if (i_res /= 44444) error stop 2
     i_res = alloc_ptr_func(i_alloc)
     if (i_res /= 77777) error stop 3
     i_res = alloc_ptr_func(i_ptr)
     if (i_res /= 44444) error stop 4

end program

      subroutine i_alloc_sub(x,y)
         implicit none
         integer,allocatable :: x
         integer :: y
         y = x
         print *, "   inside i_alloc_sub()  x=",x,"  y=",y
      end subroutine
      subroutine i_ptr_sub(x,y)
         implicit none
         integer, pointer :: x
         integer :: y
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
         integer, pointer :: x
         print *, "   inside i_ptr_func()  x=",x
         i_ptr_func = x
      end function
