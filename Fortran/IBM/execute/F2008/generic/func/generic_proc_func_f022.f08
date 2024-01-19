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
         procedure, pass(dt_var) :: proc_fsub
         generic :: procfunc_sub => proc_fsub
         procedure, pass(dt_var) :: func_ffunc
         generic :: procfunc_func => func_ffunc
   end type

   type, extends(Base) :: Child
      integer :: j /5/
      contains
         procedure, pass(dt_var) :: func_fsub, proc_ffunc
         generic :: procfunc_sub => func_fsub
         generic :: procfunc_func => proc_ffunc
   end type

contains

      function fun_arr(x)
      implicit none
          type (Base), dimension(10) :: fun_arr
          type (Base) :: x(10)

          fun_arr%i = x%i + 7
          print *,"       -- inside fun_arr()"
      end function

      type (Base) function fun_sng(x)
      implicit none
          type (Base) :: x(10)

          fun_sng%i = sum(x%i)-100
          print *,"       -- inside fun_sng()"
      end function

!***********************************************************
      subroutine proc_fsub(x, dt_var)
      implicit none
         procedure(type(base)) :: x
         class(Base), intent(inout) :: dt_var
         type (Base) :: z(10)

         z%i = dt_var%i
         z  = x(z)
         dt_var%i = sum(z%i) - 4
         print *, "   inside proc_fsub()"
      end  subroutine

      subroutine func_fsub(x, dt_var)
      implicit none
         abstract interface
            function myinterface(x)
              import Base
              type (Base), dimension(10) :: myinterface
              type (Base) :: x(10)
            end function
         end interface
         procedure(myinterface) :: x
         class(Child) :: dt_var
         type (Base)  :: z(10)

         z%i = dt_var%i + dt_var%j*10
         z = x(z)
         dt_var%i = sum(z%i)
         dt_var%j = z(5)%i + 100
         print *, "   inside func_fsub()"
      end  subroutine

!***********************************************************
      type (Base) function proc_ffunc(dt_var,y)
      implicit none
         class(Child) :: dt_var
         procedure(type (Base)) :: y
         type (Base) :: z(10)

         z%i = dt_var%i + dt_var%j*10
         proc_ffunc =  y(z)
         print *, "   inside proc_ffunc()"
      end  function


      type (Base) function func_ffunc(dt_var,y)
      implicit none
         abstract interface
            function myinterface(x)
              import Base
              type (Base), dimension(10) :: myinterface
              type (Base) :: x(10)
            end function
         end interface
         class(Base) :: dt_var
         type (Base) :: z(10)
         procedure(myinterface) :: y

         z%i = dt_var%i * 5
         z = y(z)
         func_ffunc%i = sum(z%i)
         print *, "   inside func_ffunc()"
      end  function

end module


program generic_proc_ffunc_f022
use f2008_generic
implicit none

     integer               :: i, i_res(10), i_arr(10)
     type(Base)            :: base_res
     type(Child)           :: child_res
     procedure (fun_arr), pointer :: ptr_fun_arr
     procedure (fun_sng), pointer :: ptr_fun_sng

     base_res%i  = 10; child_res%i = -100;  child_res%j = 6
     i_res = 5;  i_arr = 2

     ptr_fun_arr => fun_arr
     ptr_fun_sng => fun_sng

     call child_res%procfunc_sub(ptr_fun_arr)
     if ((child_res%i /= -330) .or. (child_res%j /= 67)) error stop 1
     call child_res%procfunc_sub(ptr_fun_sng)
     if ((child_res%i /= -34004) .or. (child_res%j /= 67)) error stop 2
     call base_res%procfunc_sub(ptr_fun_sng)
     if (base_res%i /= -4) error stop 3


     base_res%i  = 10; child_res%i = -100;  child_res%j = 6
     base_res = child_res%procfunc_func(ptr_fun_arr)
     if (base_res%i /= -4930) error stop 4
     base_res = child_res%procfunc_func(ptr_fun_sng)
     if (base_res%i /= -500) error stop 5
     base_res = base_res%procfunc_func(ptr_fun_arr)
     if (base_res%i /= -24930) error stop 6

     print *, " .... end"

end program
