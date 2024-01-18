!#######################################################################
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2008/generic/func/generic_allocatable_pointer_f006.f
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

program generic_allocatable_pointer_f006
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

     type Base
        integer               :: i_res
        integer, allocatable  :: i_alloc
        integer, pointer      :: i_ptr
     end type
     integer, target       :: i_tgt /44444/

     type(Base) :: my_b
     allocate(my_b%i_alloc)
     my_b%i_alloc = 77777
     my_b%i_ptr => i_tgt

     call alloc_ptr_sub(my_b%i_alloc, my_b%i_res)
     if (my_b%i_res /= 77777) error stop 1
     call alloc_ptr_sub(my_b%i_ptr, my_b%i_res)
     if (my_b%i_res /= 44444) error stop 2
     my_b%i_res = alloc_ptr_func(my_b%i_alloc)
     if (my_b%i_res /= 77777) error stop 3
     my_b%i_res = alloc_ptr_func(my_b%i_ptr)
     if (my_b%i_res /= 44444) error stop 4

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

