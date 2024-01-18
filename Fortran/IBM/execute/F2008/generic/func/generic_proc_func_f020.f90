!######################################################################
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2008/generic/func/generic_proc_ffunc_f020.f
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
         procedure, nopass :: proc_fsub
         generic :: procfunc_sub => proc_fsub
         procedure, nopass :: func_ffunc
         generic :: procfunc_func => func_ffunc
   end type

   type, extends(Base) :: Child
      integer :: j /5/
      contains
         procedure, nopass :: func_fsub, proc_ffunc
         generic :: procfunc_sub => func_fsub
         generic :: procfunc_func => proc_ffunc
   end type

contains

      function fun_a10(x)
      implicit none
          integer, dimension(10) :: fun_a10
          integer :: x(10)

          fun_a10 = x + 7
          print *,"       -- inside fun_a10()"
      end function

      integer function fun_int(x)
      implicit none
          integer :: x(10)

          fun_int = sum(x)-100
          print *,"       -- inside fun_int()"
      end function

!***********************************************************
      subroutine proc_fsub(x, y)
      implicit none
         procedure(integer) x
         integer :: y(10)

         y = x(y)
         print *, "   inside proc_fsub()"
      end  subroutine

      subroutine func_fsub(x, y)
      implicit none
         abstract interface
            function myinterface(x)
              integer, dimension(10) :: myinterface
              integer :: x(10)
            end function
         end interface
         procedure(myinterface) :: x
         integer :: y(10)

         y = x(y) * 7
         print *, "   inside func_fsub()"
      end  subroutine

!***********************************************************
      integer function proc_ffunc(x,y)
         integer :: x(10)
         procedure(integer) :: y

         proc_ffunc =  6 + y(x)
         x = proc_ffunc
         print *, "   inside proc_ffunc()"
      end  function


      integer function func_ffunc(x,y)
         abstract interface
            function myinterface(x)
              integer, dimension(10) :: myinterface
              integer :: x(10)
            end function
         end interface
         integer :: x(10), z(10)
         procedure(myinterface) :: y

         z = y(x)
         func_ffunc = sum(z)
         print *, "   inside func_ffunc()"
      end  function

end module


program generic_proc_ffunc_f020
use f2008_generic
implicit none

     integer               :: i, i_res(10), i_arr(10)
     type(Base)            :: base_res
     type(Child)           :: child_res
     procedure (fun_a10), pointer :: ptr_fun_a10
     procedure (fun_int), pointer :: ptr_fun_int

     base_res%i  = 10
     child_res%i = -100
     i_res = 5;  i_arr = 2

     ptr_fun_a10 => fun_a10
     ptr_fun_int => fun_int

     call child_res%procfunc_sub(ptr_fun_a10, i_arr)
     if (any(i_arr /= 63)) error stop 1
     call child_res%procfunc_sub(ptr_fun_int, i_arr)
     if (any(i_arr /= 530)) error stop 2
     call base_res%procfunc_sub(ptr_fun_int, i_arr)
     if (any(i_arr /= 5200)) error stop 3


     i_arr = 10
     i_res = child_res%procfunc_func(i_arr, ptr_fun_a10)
     if (any(i_res /= 170)) error stop 4
     i_res = child_res%procfunc_func(i_arr, ptr_fun_int)
     if (any(i_res /= 6)) error stop 5
     if (any(i_arr /= 6)) error stop 6
     i_res =base_res%procfunc_func(i_arr, ptr_fun_a10)
     if (any(i_res /= 130)) error stop 7
     if (any(i_arr /= 6)) error stop 8

     print *, " .... end"

end program
