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

program generic_proc_func_f005
implicit none

   interface procfunc_sub
      subroutine proc_fsub(x)
         procedure(integer) :: x
      end  subroutine

      subroutine func_fsub(x)
         abstract interface
            function myinterface()
              integer, dimension(10) :: myinterface
            end function
         end interface
         procedure(myinterface) :: x
      end  subroutine

      subroutine proc_ssub(x)
         abstract interface
            subroutine myinterface(y)
              integer :: y
            end subroutine
         end interface
         procedure(myinterface) :: x
      end  subroutine
   end interface

   interface procfunc_func
      integer function func_ffunc(x)
         abstract interface
            function myinterface()
              integer, dimension(10) :: myinterface
            end function
         end interface
         procedure(myinterface) :: x
      end  function

      integer function proc_ffunc(x)
         procedure(integer) :: x
      end  function

      integer function proc_sfunc(x)
         abstract interface
            subroutine myinterface(y)
              integer :: y
            end subroutine
         end interface
         procedure(myinterface) :: x
      end  function
   end interface

   interface
      function fun_a10()
          integer, dimension(10) :: fun_a10
      end function
      function fun_int()
          integer :: fun_int
      end function
      subroutine sub_int(x)
          integer :: x
      end subroutine
   end interface

     integer               :: i_res /5/
     procedure (fun_a10), pointer :: ptr_fun_a10
     procedure (fun_int), pointer :: ptr_fun_int
     procedure (sub_int), pointer :: ptr_sub_int

     ptr_fun_a10 => fun_a10
     ptr_fun_int => fun_int
     ptr_sub_int => sub_int

     call procfunc_sub(ptr_fun_a10)
     call procfunc_sub(ptr_fun_int)
     call procfunc_sub(ptr_sub_int)

     i_res = procfunc_func(ptr_fun_a10)
     if (i_res /= 70) error stop 3
     i_res = procfunc_func(ptr_fun_int)
     if (i_res /= -2) error stop 4
     i_res = procfunc_func(ptr_sub_int)
     if (i_res /= 172) error stop 5

     print *, " .... end"

end program


      function fun_a10()
      implicit none
          integer, dimension(10) :: fun_a10

          fun_a10 = 7
          print *,"       -- inside fun_a10()"
      end function

      integer function fun_int()
      implicit none

          fun_int = -8
          print *,"       -- inside fun_int()"
      end function

      subroutine sub_int(x)
      implicit none
         integer :: x

         x = x*3
          print *,"       -- inside sub_int()"
      end subroutine

!***********************************************************
      subroutine proc_fsub(x)
      implicit none
         procedure(integer) x
         integer :: y

         y = x()
         print *, "   inside proc_fsub()    y=",y
      end  subroutine

      subroutine func_fsub(x)
      implicit none
         abstract interface
            function myinterface()
              integer, dimension(10) :: myinterface
            end function
         end interface
         procedure(myinterface) :: x
         integer :: y(10)

         y = x() * 7
         print *, "   inside func_fsub()  y=",y
      end  subroutine

      subroutine proc_ssub(x)
      implicit none
         abstract interface
            subroutine myinterface(y)
              integer :: y
            end subroutine
         end interface
         procedure(myinterface) :: x
         integer :: y

         y = -100
         call x(y)
         print *, "   inside proc_ssub()    y=",y
      end  subroutine

!***********************************************************
      integer function proc_ffunc(x)
      implicit none
         procedure(integer) :: x

         proc_ffunc = 6 + x()
         print *, "   inside proc_ffunc()"
      end  function

      integer function func_ffunc(x)
      implicit none
         abstract interface
            function myinterface()
              integer, dimension(10) :: myinterface
            end function
         end interface
         procedure(myinterface) :: x
         integer :: y(10)

         y = x()
         func_ffunc = sum(y)
         print *, "   inside func_ffunc()  y=",y
      end  function

      integer function proc_sfunc(x)
      implicit none
         abstract interface
            subroutine myinterface(y)
              integer :: y
            end subroutine
         end interface
         procedure(myinterface) :: x
         integer :: y /55/

         call x(y)
         proc_sfunc = y + 7
         print *, "   inside proc_sfunc()"
      end  function

