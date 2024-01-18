!######################################################################
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2008/generic/func/generic_func_func_f022.f
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
         abstract interface
            function myinterface(x)
              integer (KIND=4), DIMENSION(3)  :: myinterface
              integer :: x
            end function
         end interface
         class(Base), intent(inout) :: x
         procedure(myinterface) :: y

         print *, "    inside fsub_1()"
         x%i = sum(y(x%i))
      end subroutine

      subroutine  fsub_2 ( x, y )
      implicit none
         abstract interface
            function myinterface(x)
              integer (KIND=4), DIMENSION(3,3)  :: myinterface
              integer :: x
            end function
         end interface
         class(Base), intent(inout) :: x
         procedure(myinterface) :: y

         print *, "    inside fsub_2()"
         x%i = sum(y(x%i))
      end subroutine

      integer function ffunc_1 ( x, y )
      implicit none
         abstract interface
            function myinterface(x)
              integer (KIND=4), DIMENSION(3)  :: myinterface
              integer :: x
            end function
         end interface
         class(Base), intent(inout) :: x
         procedure(myinterface) :: y

         print *, "    inside ffunc_1()"
         x%i = sum(y(x%i))
         ffunc_1 = x%i
      end function

      integer function ffunc_2 ( x, y )
      implicit none
         abstract interface
            function myinterface(x)
              integer (KIND=4), DIMENSION(3,3)  :: myinterface
              integer :: x
            end function
         end interface
         class(Base), intent(inout) :: x
         procedure(myinterface) :: y

         print *, "    inside ffunc_2()"
         x%i = sum(y(x%i))
         ffunc_2 = x%i
      end function

      function func_rank1(x)
      implicit none
         integer (KIND=4), DIMENSION(3) :: func_rank1
         integer :: x
         
         print *, "       inside func_rank1()"
         func_rank1 = x*4 -5
      end function

      function func_rank2(x)
      implicit none
         integer (KIND=4), DIMENSION(3,3) :: func_rank2
         integer :: x

         print *, "       inside func_rank2()"
         func_rank2 = x-64
      end function

end module


program generic_func_func_f022
use f2008_generic
implicit none

         abstract interface
            function myinterface_1r(x)
              integer (KIND=4), DIMENSION(3)  :: myinterface_1r
              integer :: x
            end function
            function myinterface_2r(x)
              integer (KIND=4), DIMENSION(3,3)  :: myinterface_2r
              integer :: x
            end function
         end interface

   type(Base) :: base_res1
   integer    :: i_res
   procedure(myinterface_1r), pointer :: ptr_func_rank1
   procedure(myinterface_2r), pointer :: ptr_func_rank2

   ptr_func_rank1 => func_rank1
   ptr_func_rank2 => func_rank2

   ! call generic type bound
   base_res1%i = 100; i_res = 0
   call base_res1%funcfunc_sub(func_rank1)
   if (base_res1%i /= 1185)   error stop 1
   call base_res1%funcfunc_sub(func_rank2)
   if (base_res1%i /= 10089)   error stop 2
   base_res1%i = 10; i_res = 0
   i_res = base_res1%funcfunc_func(func_rank1)
   if ((base_res1%i /= 105) .or. (i_res /= 105))   error stop 3
   i_res = base_res1%funcfunc_func(func_rank2)
   if ((base_res1%i /= 369) .or. (i_res /= 369))   error stop 4

   base_res1%i = 100; i_res = 0
   call base_res1%funcfunc_sub(ptr_func_rank1)
   if (base_res1%i /= 1185)   error stop 11
   call base_res1%funcfunc_sub(ptr_func_rank2)
   if (base_res1%i /= 10089)   error stop 12
   base_res1%i = 10; i_res = 0
   i_res = base_res1%funcfunc_func(ptr_func_rank1)
   if ((base_res1%i /= 105) .or. (i_res /= 105))   error stop 13
   i_res = base_res1%funcfunc_func(ptr_func_rank2)
   if ((base_res1%i /= 369) .or. (i_res /= 369))   error stop 14

end program
