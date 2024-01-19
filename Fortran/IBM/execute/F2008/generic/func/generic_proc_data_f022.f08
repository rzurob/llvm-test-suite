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
         procedure, pass(dt_var) :: proc_sub
         generic :: proc_data_sub => proc_sub
         procedure, pass(dt_var) :: data_func
         generic :: proc_data_func => data_func
   end type

   type, extends(Base) :: Child
      integer :: j /5/
      contains
         procedure, pass(dt_var) :: data_sub, proc_func
         generic :: proc_data_sub => data_sub
         generic :: proc_data_func => proc_func
   end type

   contains

     type (Base) function fun1(x)
      implicit none
         class(Base) :: x
         x%i =  x%i + 77
         fun1 = x
         print *,"       inside fun1 ()"
      end function

      subroutine proc_sub(x, dt_var)
      implicit none
         abstract interface
            function myinterface(x)
               import :: Base
               type(Base)  :: myinterface
               class(Base) :: x
            end function
         end interface
         procedure(myinterface) :: x
         class(Base), intent(inout) :: dt_var

         type(Base) :: y

         y = x(dt_var)
         print *, "   inside proc_sub()"
      end  subroutine

      subroutine data_sub(x, dt_var)
      implicit none
         type(Base) :: x
         class(Child) :: dt_var

         x%i = (x%i * 7) + dt_var%i + dt_var%j
         print *, "   inside data_sub()"
      end  subroutine

      type(Base) function proc_func(x, dt_var)
         implicit none
         abstract interface
            function myinterface(x)
               import :: Base
               type(Base)  :: myinterface
               class(Base) :: x
            end function
         end interface
         procedure(myinterface) :: x
         class(Child) :: dt_var

         proc_func = x(dt_var)
         print *, "   inside proc_func()"
      end  function

      type(Base) function data_func(x, dt_var)
         implicit none
         type(Base)  :: x
         class(Base) :: dt_var

         x%i =  x%i - dt_var%i
         data_func%i = x%i*3+2
         print *, "   inside data_func()"
      end  function

end module

program generic_proc_data_f022
use f2008_generic
implicit none

     type(Base)            :: base_res, base_res2
     type(Child)           :: child_res

     base_res%i  = 10;     base_res2%i = -7
     child_res%i = -100

     call child_res%proc_data_sub(base_res)
     if (base_res%i /= -25) error stop 1
     call child_res%proc_data_sub(fun1)
     if (child_res%i /= -23) error stop 2
     call base_res%proc_data_sub(fun1)
     if (base_res%i /= 52) error stop 3
     base_res2  = base_res%proc_data_func(base_res)
     if (base_res%i /= 0) error stop 4
     if (base_res2%i /= 2) error stop 5
     base_res%i  = 44
     base_res2 = child_res%proc_data_func(base_res)
     if (base_res%i /= 67) error stop 6
     if (base_res2%i /= 203) error stop 7
     base_res2 =child_res%proc_data_func(fun1)
     if (base_res2%i /= 54) error stop 5
     print *, " .... end"

end program
