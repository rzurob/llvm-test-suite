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

   interface proc_data_sub
      module procedure proc_sub
      module procedure data_sub
   end interface

   interface proc_data_func
      module procedure data_func
      module procedure proc_func
   end interface

   integer               :: i_res /5/

contains
      integer function fun1()
      implicit none
          print *,"       inside fun1 ()"
          fun1 = int(7654.32)
      end function

      subroutine proc_sub(x)
      implicit none
         procedure(integer) x
         integer :: y

         y = x()
         print *, "   inside proc_sub()    y=",y
      end  subroutine

      subroutine data_sub(x)
      implicit none
         integer :: x

         x = x * 7
         print *, "   inside data_sub()  x=",x
      end  subroutine

      integer function proc_func(x)
         implicit none
         procedure(integer) :: x

         proc_func = 6 + x()
         print *, "   inside proc_func()"
      end  function

      integer function data_func(x)
         implicit none
         integer :: x

         data_func = x * 3
         print *, "   inside data_func()  x=",x
      end  function

      subroutine my_execute()
      implicit none

         call proc_data_sub(i_res)
         if (i_res /= 35) error stop 1
         call proc_data_sub(fun1)
         i_res = proc_data_func(i_res)
         if (i_res /= 105) error stop 3
         i_res = proc_data_func(fun1)
         if (i_res /= 7660) error stop 4
      end  subroutine

end module

program generic_proc_data_f002
use f2008_generic
implicit none

     call my_execute()
     print *, " .... end"

end program