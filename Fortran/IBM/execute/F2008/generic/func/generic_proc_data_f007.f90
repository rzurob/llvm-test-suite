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

program generic_proc_data_f007
implicit none

   interface proc_data_sub
      subroutine proc_sub(x,y)
         abstract interface
            function myinterface(x)
              integer :: x(10)
              integer, dimension(10) :: myinterface
            end function
         end interface
         procedure(myinterface) ::  y
         integer, pointer :: x(:)
      end  subroutine

      subroutine data_sub(x,y)
         integer, allocatable :: x(:)
         integer :: y(10)
      end  subroutine
   end interface

   interface proc_data_func
      function data_func(x,y)
         integer, dimension(10) :: data_func
         integer :: x(10)
         integer, pointer :: y(:)
      end  function

      function proc_func(x,y)
         abstract interface
            function myinterface(x)
              integer :: x(10)
              integer, dimension(10) :: myinterface
            end function
         end interface
         procedure(myinterface) ::  x
         integer :: y(10)
         integer, dimension(10) :: proc_func
      end  function
   end interface

   interface
      function fun1(x)
          integer :: x(10)
          integer, dimension(10) :: fun1
      end function
   end interface

     integer               :: i, i_res(10), i_arr(10)
     integer, target       :: i_tgt(10)
     integer, allocatable  :: i_alloc(:)
     integer, pointer      :: i_ptr(:)
     procedure (fun1), pointer :: p_fun1

     allocate(i_alloc(10))
     i_res = 5;   i_alloc = 77;   i_tgt = 44;   i_arr = 2
     i_ptr => i_tgt
     p_fun1 => fun1

     call proc_data_sub(i_alloc, i_res)
     if (any(i_res /= 72)) error stop 1
     call proc_data_sub(i_ptr, fun1)
     if (any(i_ptr /= 50)) error stop 2
     i_res = proc_data_func(i_arr, i_ptr)
     if (any(i_res /= -144)) error stop 3
     i_res = proc_data_func(p_fun1, i_alloc)
     if (any(i_res /= 85)) error stop 4
     print *, " .... end"

end program


      function fun1(x)
      implicit none
          integer :: x(10)
          integer, dimension(10) :: fun1
          fun1 = x + 2
          print *,"       inside fun1 ()"
      end function

      subroutine proc_sub(x,y)
      implicit none
         abstract interface
            function myinterface(x)
              integer :: x(10)
              integer, dimension(10) :: myinterface
            end function
         end interface
         procedure(myinterface) ::  y
         integer, pointer :: x(:)

         x = y(x) + 4
         print *, "   inside proc_sub()"
      end  subroutine

      subroutine data_sub(x, y)
      implicit none
         integer, allocatable :: x(:)
         integer :: y(10)

         y = x-y
         print *, "   inside data_sub()"
      end  subroutine

      function proc_func(x,y)
      implicit none
         abstract interface
            function myinterface(x)
              integer :: x(10)
              integer, dimension(10) :: myinterface
            end function
         end interface
         procedure(myinterface) ::  x
         integer :: y(10)
         integer, dimension(10) :: proc_func

         proc_func = 6 + x(y)
         print *, "   inside proc_func()"
      end  function

      function data_func(x,y)
      implicit none
         integer :: x(10)
         integer, pointer :: y(:)
         integer, dimension(10) :: data_func

         data_func = (x-y)*3
         print *, "   inside data_func()"
      end  function

