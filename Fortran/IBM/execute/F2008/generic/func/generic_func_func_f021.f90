!######################################################################
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2008/generic/func/generic_func_func_f021.f
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
         procedure, pass(x) :: fsub_1
         procedure, pass    :: fsub_2
         generic :: funcfunc_sub => fsub_1, fsub_2
         procedure, pass    :: ffunc_1, ffunc_2
         generic :: funcfunc_func => ffunc_1, ffunc_2
   end type

contains

      subroutine fsub_1 ( x, y )
      implicit none
         class(Base), intent(inout) :: x
         procedure(integer*4) :: y

         print *, "    inside fsub_1()"
         x%i = y(x%i)
      end subroutine

      subroutine  fsub_2 ( x, y )
      implicit none
         class(Base), intent(inout) :: x
         procedure(integer*8) :: y

         print *, "    inside fsub_2()"
         x%i = int(y(x%i),kind(0))
      end subroutine

      integer function ffunc_1 ( x, y )
      implicit none
         class(Base), intent(inout) :: x
         procedure(integer*4) :: y

         print *, "    inside ffunc_1()"
         x%i = y(x%i)
         ffunc_1 = x%i
      end function

      integer function ffunc_2 ( x, y )
      implicit none
         class(Base), intent(inout) :: x
         procedure(integer*8) :: y

         print *, "    inside ffunc_2()"
         x%i = int(y(x%i),kind(0))
         ffunc_2 = x%i
      end function

      integer*4 function func_int4(x)
      implicit none
         integer x
         
         print *, "       inside func_int4()"
         func_int4 = x*4 -5
      end function

     integer*8  function func_int8(x)
      implicit none
         integer x

         print *, "       inside func_int8()"
         func_int8 = x-64
      end function

end module


program generic_func_func_f021
use f2008_generic
implicit none

   type(Base) :: base_res1
   integer    :: i_res
   procedure(integer*4), pointer :: ptr_func_int4
   procedure(integer*8), pointer :: ptr_func_int8

   ptr_func_int4 => func_int4
   ptr_func_int8 => func_int8

   base_res1%i = 100; i_res = 0

   ! call generic type bound
   call base_res1%funcfunc_sub(func_int4)
   if (base_res1%i /= 395)   error stop 1
   call base_res1%funcfunc_sub(func_int8)
   if (base_res1%i /= 331)   error stop 2
   i_res = base_res1%funcfunc_func(func_int4)
   if ((base_res1%i /= 1319) .or. (i_res /= 1319))   error stop 3
   i_res = base_res1%funcfunc_func(func_int8)
   if ((base_res1%i /= 1255) .or. (i_res /= 1255))   error stop 4

   base_res1%i = 100; i_res = 0
   call base_res1%funcfunc_sub(ptr_func_int4)
   if (base_res1%i /= 395)   error stop 11
   call base_res1%funcfunc_sub(ptr_func_int8)
   if (base_res1%i /= 331)   error stop 12
   i_res = base_res1%funcfunc_func(ptr_func_int4)
   if ((base_res1%i /= 1319) .or. (i_res /= 1319))   error stop 13
   i_res = base_res1%funcfunc_func(ptr_func_int8)
   if ((base_res1%i /= 1255) .or. (i_res /= 1255))   error stop 14

end program
