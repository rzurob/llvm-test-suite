!######################################################################
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2008/generic/func/generic_proc_data_f021.f
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
         procedure, nopass :: proc_sub
         generic :: proc_data_sub => proc_sub
         procedure, nopass :: data_func
         generic :: proc_data_func => data_func
   end type

   type, extends(Base) :: Child
      contains
         procedure, nopass :: data_sub, proc_func 
         generic :: proc_data_sub => data_sub
         generic :: proc_data_func => proc_func
   end type

   contains

      type(Base) function fun1()
      implicit none
          print *,"       inside fun1 ()"
          fun1 = Base(100)
      end function

      subroutine proc_sub(x)
      implicit none
         abstract interface
            function myinterface()
               import :: Base
               type(Base) :: myinterface
            end function
         end interface
         procedure(myinterface) :: x

         type(Base) :: y

         y = x()
         print *, "   inside proc_sub()"
      end  subroutine

      subroutine data_sub(x)
      implicit none
         type(Base) :: x

         x%i = x%i * 7
         print *, "   inside data_sub()"
      end  subroutine

      type(Base) function proc_func(x)
         implicit none
         procedure(type(Base)) :: x

         proc_func = x()
         print *, "   inside proc_func()"
      end  function

      type(Base) function data_func(x)
         implicit none
         type(Base) :: x

         data_func%i = x%i * 3
         print *, "   inside data_func()"
      end  function

end module

program generic_proc_data_f021
use f2008_generic
implicit none

     type(Base)            :: base_res, base_res2
     type(Child)           :: child_res

     base_res%i  = 10
     child_res%i = -100

     call child_res%proc_data_sub(base_res)
     if (base_res%i /= 70) error stop 1
     call child_res%proc_data_sub(fun1)
     call base_res%proc_data_sub(fun1)
     base_res2  = base_res%proc_data_func(base_res)
     if (base_res2%i /= 210) error stop 3
     base_res%i  = 44
     base_res2 = child_res%proc_data_func(base_res)
     if (base_res2%i /= 132) error stop 4
     base_res2 =child_res%proc_data_func(fun1)
     if (base_res2%i /= 100) error stop 5

     associate (fromfun => fun1())
        base_res2 =child_res%proc_data_func(fromfun)
     end associate
     if (base_res2%i /= 300) error stop 6

     print *, " .... end"

end program
