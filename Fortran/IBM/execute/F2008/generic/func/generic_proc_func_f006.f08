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

program generic_proc_func_f006
implicit none

   interface procfunc_sub
      subroutine proc_fsub(x,y)
         procedure(integer) :: x
         integer :: y(10)
      end  subroutine

      subroutine func_fsub(x,y)
         abstract interface
            function myinterface(x)
              integer, dimension(10) :: myinterface
              integer :: x(10)
            end function
         end interface
         procedure(myinterface) :: x
         integer :: y(10)
      end  subroutine

      subroutine proc_ssub(x,y)
         abstract interface
            subroutine myinterface(x, y)
              integer :: x
              integer :: y(10)
            end subroutine
         end interface
         procedure(myinterface) :: x
         integer :: y(10)
      end  subroutine
   end interface

   interface procfunc_func
      integer function func_ffunc(x,y)
         abstract interface
            function myinterface(x)
              integer, dimension(10) :: myinterface
              integer :: x(10)
            end function
         end interface
         integer :: x(10)
         procedure(myinterface) :: y
      end  function

      integer function proc_ffunc(x,y)
         integer :: x(10)
         procedure(integer) :: y
      end  function

      integer function proc_sfunc(x,y)
         abstract interface
            subroutine myinterface(x, y)
              integer :: x
              integer :: y(10)
            end subroutine
         end interface
         integer :: x(10)
         procedure(myinterface) :: y
      end  function
   end interface

   interface
      function fun_a10(x)
          integer, dimension(10) :: fun_a10
          integer :: x(10)
      end function
      function fun_int(x)
          integer :: fun_int
          integer :: x(10)
      end function
      subroutine sub_int(x,y)
          integer :: x
          integer :: y(10)
      end subroutine
   end interface

     integer               :: i, i_res(10), i_arr(10)
     procedure (fun_a10), pointer :: ptr_fun_a10
     procedure (fun_int), pointer :: ptr_fun_int
     procedure (sub_int), pointer :: ptr_sub_int

     i_res = 5;  i_arr = 2

     ptr_fun_a10 => fun_a10
     ptr_fun_int => fun_int
     ptr_sub_int => sub_int

     call procfunc_sub(ptr_fun_a10, i_arr)
     if (any(i_arr /= 63)) error stop 1
     call procfunc_sub(ptr_fun_int, i_arr)
     if (any(i_arr /= 530)) error stop 2
     call procfunc_sub(ptr_sub_int, i_arr)
     if (any(i_arr /= 16430)) error stop 3

     i_arr = 10
     i_res = procfunc_func(i_arr, ptr_fun_a10)
     if (any(i_res /= 170)) error stop 4
     i_res = procfunc_func(i_arr, ptr_fun_int)
     if (any(i_res /= 6)) error stop 5
     if (any(i_arr /= 6)) error stop 6
     i_res = procfunc_func(i_arr, ptr_sub_int)
     if (any(i_res /= 193)) error stop 7
     if (any(i_arr /= 186)) error stop 8

     print *, " .... end"

end program


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

      subroutine sub_int(x, y)
      implicit none
         integer :: x
         integer :: y(10)

         x = (sum(y)*3) + y(5)
          print *,"       -- inside sub_int()"
      end subroutine

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

      subroutine proc_ssub(x,y)
         abstract interface
            subroutine myinterface(x, y)
              integer :: x
              integer :: y(10)
            end subroutine
         end interface
         procedure(myinterface) :: x
         integer :: y(10), z

         z = -100
         call x(z, y)
         y = z
         print *, "   inside proc_ssub()"
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


      integer function proc_sfunc(x,y)
         abstract interface
            subroutine myinterface(x, y)
              integer :: x
              integer :: y(10)
            end subroutine
         end interface
         integer :: x(10)
         procedure(myinterface) :: y
         integer :: z /55/

         call y(z, x)
         x = z
         proc_sfunc = z + 7
         print *, "   inside proc_sfunc()"
      end  function

