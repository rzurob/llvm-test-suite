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

program generic_proc_data_f005
implicit none

   interface proc_data_sub
      subroutine proc_sub(x)
         integer, external :: x
      end  subroutine

      subroutine data_sub(x)
         integer :: x
      end  subroutine
   end interface

   interface proc_data_func
      integer function data_func(x)
         integer :: x
      end  function

      integer function proc_func(x)
         integer, external :: x
      end  function
   end interface

   interface
      integer function fun1()
      end function
   end interface

     integer               :: i_res /5/
     integer, target       :: i_tgt /-44/
     integer, pointer      :: i_ptr
     procedure (fun1), pointer :: p_fun1

     i_ptr => i_tgt
     p_fun1 => fun1

     call proc_data_sub(i_res)
     if (i_res /= 35) error stop 1
     call proc_data_sub(p_fun1)
     i_res = proc_data_func(i_ptr)
     if (i_res /= -132) error stop 3
     i_res = proc_data_func(p_fun1)
     if (i_res /= 7660) error stop 4

     call proc_data_sub(100)
     i_res = proc_data_func(10)
     if (i_res /= 30) error stop 5

     print *, " .... end"

end program


      integer function fun1()
      implicit none
          print *,"       inside fun1 ()"
          fun1 = int(7654.32)
      end function

      subroutine proc_sub(x)
      implicit none
         integer, external :: x
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
         integer, external :: x

         proc_func = 6 + x()
         print *, "   inside proc_func()"
      end  function

      integer function data_func(x)
      implicit none
         integer :: x

         data_func = x * 3
         print *, "   inside data_func()  x=",x
      end  function

