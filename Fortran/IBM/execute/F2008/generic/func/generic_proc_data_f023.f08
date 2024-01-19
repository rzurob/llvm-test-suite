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
         procedure, pass :: proc_sub
         generic :: proc_data_sub => proc_sub
         procedure, pass :: data_func
         generic :: proc_data_func => data_func
   end type

   type, extends(Base) :: Child
      integer :: j /5/
      contains
         procedure, pass :: data_sub, proc_func
         generic :: proc_data_sub => data_sub
         generic :: proc_data_func => proc_func
   end type

   contains

     type (Base) function someproc(x)
      implicit none
         class(Base) :: x
         x%i =  x%i + 77
         someproc = x
         print *,"       inside someproc ()"
      end function

      subroutine proc_sub(x, y)
      implicit none
         abstract interface
            function myinterface(y)
               import :: Base
               type(Base)  :: myinterface
               class(Base) :: y
            end function
         end interface
         procedure(myinterface) :: y
         class(Base), intent(inout) :: x

         type(Base) :: z

         z = y(x)
         print *, "   inside proc_sub()"
      end  subroutine

      subroutine data_sub(n, m)
      implicit none
         type(Base) :: m
         class(Child) :: n

         m%i = (m%i * 7) + n%i + n%j
         print *, "   inside data_sub()"
      end  subroutine

      type(Base) function proc_func(y, x)
         implicit none
         abstract interface
            function myinterface(x)
               import :: Base
               type(Base)  :: myinterface
               class(Base) :: x
            end function
         end interface
         procedure(myinterface) :: x
         class(Child) :: y

         proc_func = x(y)
         print *, "   inside proc_func()"
      end  function

      type(Base) function data_func(z, x)
         implicit none
         type(Base)  :: x
         class(Base) :: z

         x%i =  x%i - z%i
         data_func%i = x%i*3+2
         print *, "   inside data_func()"
      end  function

end module

program generic_proc_data_f023
use f2008_generic
implicit none

     type(Base)            :: base_res, base_res2
     type(Child)           :: child_res
     procedure (someproc), pointer :: ptr_someproc

     base_res%i  = 10;     base_res2%i = -7
     child_res%i = -100
     ptr_someproc => someproc

     call child_res%proc_data_sub(base_res)
     if (base_res%i /= -25) error stop 1
     call child_res%proc_data_sub(someproc)
     if (child_res%i /= -23) error stop 2
     call base_res%proc_data_sub(ptr_someproc)
     if (base_res%i /= 52) error stop 3
     base_res2  = base_res%proc_data_func(base_res)
     if (base_res%i /= 0) error stop 4
     if (base_res2%i /= 2) error stop 5
     base_res%i  = 44
     base_res2 = child_res%proc_data_func(base_res)
     if (base_res%i /= 67) error stop 6
     if (base_res2%i /= 203) error stop 7
     base_res2 = child_res%proc_data_func(ptr_someproc)
     if (base_res2%i /= 54) error stop 5
     print *, " .... end"

end program
