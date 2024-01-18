!#######################################################################
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2008/generic/func/generic_allocatable_pointer_f010.f
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

   interface alloc_ptr_sub
      subroutine i_alloc_sub(x,y)
         integer,allocatable ::x
         integer y
      end  subroutine
      subroutine i_ptr_sub(x,y)
         integer, pointer ::x
         integer y
      end  subroutine

      subroutine ch_ptr_sub(x,y)
         character(KIND=1,LEN=10), pointer ::x
         character(KIND=1,LEN=10) y
      end  subroutine
      subroutine ch_alloc_sub(x,y)
         character(KIND=1,LEN=10),allocatable ::x
         character(KIND=1,LEN=10) y
      end  subroutine
   end interface

   interface alloc_ptr_func
      integer function i_alloc_func(x)
         integer,allocatable ::x
      end  function
      integer function i_ptr_func(x)
         integer, pointer::x
      end  function

      character(KIND=1,LEN=10) function ch_ptr_func(x)
         character(KIND=1,LEN=10), pointer::x
      end  function
      character(KIND=1,LEN=10) function ch_alloc_func(x)
         character(KIND=1,LEN=10),allocatable ::x
      end  function
   end interface

     integer               :: i_res
     integer, target       :: i_tgt /44444/
     integer, allocatable  :: i_alloc
     integer, pointer      :: i_ptr
     character(KIND=1,LEN=10)               :: ch_res
     character(KIND=1,LEN=10), target       :: ch_tgt /'pin point'/
     character(KIND=1,LEN=10), allocatable  :: ch_alloc
     character(KIND=1,LEN=10), pointer      :: ch_ptr

end module


program generic_allocatable_pointer_f010
use f2008_generic
implicit none

     allocate(i_alloc)
     i_alloc = 77777
     i_ptr => i_tgt

     call alloc_ptr_sub(i_alloc, i_res)
     if (i_res /= 77777) error stop 1
     call alloc_ptr_sub(i_ptr, i_res)
     if (i_res /= 44444) error stop 2
     call alloc_ptr_sub(y=i_res, x=i_alloc)
     if (i_res /= 77777) error stop 11
     call alloc_ptr_sub(y=i_res, x=i_ptr)
     if (i_res /= 44444) error stop 22
     i_res = alloc_ptr_func(i_alloc)
     if (i_res /= 77777) error stop 3
     i_res = alloc_ptr_func(i_ptr)
     if (i_res /= 44444) error stop 4


     allocate(ch_alloc)
     ch_alloc = 'a_locate'
     ch_ptr => ch_tgt

     call alloc_ptr_sub(ch_alloc, ch_res)
     if (ch_res /= 'a_locate') error stop 1
     call alloc_ptr_sub(ch_ptr, ch_res)
     if (ch_res /= 'pin point') error stop 2
     call alloc_ptr_sub(y=ch_res, x=ch_alloc)
     if (ch_res /= 'a_locate') error stop 11
     call alloc_ptr_sub(y=ch_res, x=ch_ptr)
     if (ch_res /= 'pin point') error stop 22
     ch_res = alloc_ptr_func(ch_alloc)
     if (ch_res /= 'a_locate') error stop 3
     ch_res = alloc_ptr_func(ch_ptr)
     if (ch_res /= 'pin point') error stop 4

end program

      subroutine i_alloc_sub(x,y)
         implicit none
         integer,allocatable ::x
         integer y
         y = x
         print *, "   inside i_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine i_ptr_sub(x,y)
         implicit none
         integer, pointer ::x
         integer y
         y = x
         print *, "   inside i_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      integer function i_alloc_func(x)
         implicit none
         integer,allocatable ::x
         print *, "   inside i_alloc_func()  x=",x
         i_alloc_func = x
      end  function
      integer function i_ptr_func(x)
         implicit none
         integer, pointer::x
         print *, "   inside i_ptr_func()  x=",x
         i_ptr_func = x
      end  function


      subroutine ch_alloc_sub(x,y)
         implicit none
         character(KIND=1,LEN=10),allocatable ::x
         character(KIND=1,LEN=10) y
         y = x
         print *, "   inside ch_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine ch_ptr_sub(x,y)
         implicit none
         character(KIND=1,LEN=10), pointer ::x
         character(KIND=1,LEN=10) y
         y = x
         print *, "   inside ch_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      character(KIND=1,LEN=10) function ch_alloc_func(x)
         implicit none
         character(KIND=1,LEN=10),allocatable ::x
         print *, "   inside ch_alloc_func()  x=",x
         ch_alloc_func = x
      end  function
      character(KIND=1,LEN=10) function ch_ptr_func(x)
         implicit none
         character(KIND=1,LEN=10), pointer::x
         print *, "   inside ch_ptr_func()  x=",x
         ch_ptr_func = x
      end  function

