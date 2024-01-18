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

program generic_proc_data_f006
implicit none

   interface proc_data_sub
      subroutine proc_sub(x,y)
         integer, pointer :: x
         procedure(integer) :: y
      end  subroutine

      subroutine data_sub(x,y)
         integer, allocatable :: x
         integer :: y
      end  subroutine
   end interface

   interface proc_data_func
      integer function data_func(x,y)
         integer :: x
         integer, pointer :: y
      end  function

      integer function proc_func(x,y)
         procedure(integer) :: x
         integer :: y
      end  function
   end interface

   interface
      integer function fun1(x)
          integer, intent(in) :: x
      end function
   end interface

     integer               :: i, i_res /5/
     integer               :: i_arr(10) = (/ (i, i = 1, 10 ) /)
     integer, target       :: i_tgt /44/
     integer, allocatable  :: i_alloc
     integer, pointer      :: i_ptr
     procedure (fun1), pointer :: p_fun1

     allocate(i_alloc)
     i_alloc = 77
     i_ptr => i_tgt
     p_fun1 => fun1

     call proc_data_sub(i_alloc, i_res)
     if (i_res /= 534) error stop 1
     call proc_data_sub(i_ptr, p_fun1)
     if (i_ptr /= 7788) error stop 2
     i_res = proc_data_func(i_arr(5), i_ptr)
     if (i_res /= -23349) error stop 3
     i_res = proc_data_func(p_fun1, i_alloc)
     if (i_res /= 5935) error stop 4
     print *, " .... end"

end program


      integer function fun1(x)
      implicit none
          integer, intent(in) :: x
          fun1 = x**2
          print *,"       inside fun1 ()"
      end function

      subroutine proc_sub(x,y)
      implicit none
         integer, pointer :: x
         procedure(integer) :: y

          x = x+ (y(x) * 4)
         print *, "   inside proc_sub()"
      end  subroutine

      subroutine data_sub(x, y)
      implicit none
         integer, allocatable :: x
         integer :: y

         y = x * 7 - y
         print *, "   inside data_sub()"
      end  subroutine

      integer function proc_func(x,y)
      implicit none
         procedure(integer) :: x
         integer :: y

         proc_func = 6 + x(y)
         print *, "   inside proc_func()"
      end  function

      integer function data_func(x,y)
      implicit none
         integer :: x
         integer, pointer :: y

         data_func = (x-y)*3
         print *, "   inside data_func()"
      end  function

