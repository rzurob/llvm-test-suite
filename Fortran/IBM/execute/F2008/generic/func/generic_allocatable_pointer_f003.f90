!#######################################################################
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2008/generic/func/generic_allocatable_pointer_f003.f
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

program generic_allocatable_pointer_f003
implicit none

   interface alloc_ptr_sub
      subroutine i_alloc_sub(x,y)
         integer (KIND=4), DIMENSION(:),allocatable :: x
         integer (KIND=4), DIMENSION(3) :: y
      end  subroutine
      subroutine i_ptr_sub(x,y)
         integer (KIND=4), DIMENSION(:), pointer :: x
         integer (KIND=4), DIMENSION(3) :: y
      end  subroutine
   end interface

   interface alloc_ptr_func
      function i_ptr_func(x) result(res_i_ptr_func)
         integer (KIND=4), DIMENSION(3) :: res_i_ptr_func
         integer (KIND=4), DIMENSION(:), pointer :: x
      end  function
      function i_alloc_func(x) result(res_i_alloc_func)
         integer (KIND=4), DIMENSION(3) :: res_i_alloc_func
         integer (KIND=4), DIMENSION(:),allocatable :: x
      end  function
   end interface

     integer (KIND=4), DIMENSION(3)               :: i_res
     integer (KIND=4), DIMENSION(3), target       :: i_tgt /1, -3, 5/
     integer (KIND=4), DIMENSION(:), allocatable  :: i_alloc
     integer (KIND=4), DIMENSION(:), pointer      :: i_ptr

     integer (KIND=4), DIMENSION(3)               :: i_resp /1, -3, 5/
     integer (KIND=4), DIMENSION(3)               :: i_resa /900, 800, 700/
     allocate(i_alloc(3), source = (/900, 800, 700/))
     i_ptr => i_tgt

     call alloc_ptr_sub(i_alloc, i_res)
     if (ANY(i_res /= i_resa)) error stop 1
     call alloc_ptr_sub(i_ptr, i_res)
     if (ANY(i_res /= i_resp)) error stop 2

     i_res = alloc_ptr_func(i_alloc)
     if (ANY(i_res /= i_resa)) error stop 3
     i_res = alloc_ptr_func(i_ptr)
     if (ANY(i_res /= i_resp)) error stop 4
     print *, " .... end"

end program

      subroutine i_alloc_sub(x,y)
         implicit none
         integer (KIND=4), DIMENSION(:),allocatable :: x
         integer (KIND=4), DIMENSION(3) :: y
         y = x
         print *, "   inside i_alloc_sub()  x=",x,"  y=",y
      end  subroutine
      subroutine i_ptr_sub(x,y)
         implicit none
         integer (KIND=4), DIMENSION(:), pointer :: x
         integer (KIND=4), DIMENSION(3) :: y
         y = x
         print *, "   inside i_ptr_sub()  x=",x,"  y=",y
      end  subroutine
      function i_alloc_func(x) result(res_i_alloc_func)
         implicit none
         integer (KIND=4), DIMENSION(3) :: res_i_alloc_func
         integer (KIND=4), DIMENSION(:),allocatable :: x
         print *, "   inside i_alloc_func()  x=",x
         res_i_alloc_func = x
      end  function
      function i_ptr_func(x) result(res_i_ptr_func)
         implicit none
         integer (KIND=4), DIMENSION(3) :: res_i_ptr_func
         integer (KIND=4), DIMENSION(:), pointer :: x
         print *, "   inside i_ptr_func()  x=",x
         res_i_ptr_func = x
      end  function

