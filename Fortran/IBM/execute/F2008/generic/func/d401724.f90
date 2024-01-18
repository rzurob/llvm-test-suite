program generic_proc_func_f005
implicit none

   interface procfunc_sub
      subroutine proc_fsub(x)
!         procedure(integer) :: x
         integer, external :: x
      end  subroutine

      subroutine func_fsub(x)
         abstract interface
            function myinterface()
              integer, dimension(10) :: myinterface
            end function
         end interface
         procedure(myinterface) :: x
      end  subroutine
   end interface

   interface
      function fun_a10()
          integer, dimension(10) :: fun_a10
      end function
      function fun_int()
          integer :: fun_int
      end function
   end interface

     call procfunc_sub(fun_a10)
     call procfunc_sub(fun_int)
end program

      function fun_a10()
      implicit none
          integer, dimension(10) :: fun_a10
          print *,"       -- inside fun_a10()"
          fun_a10 = 7
      end function

      integer function fun_int()
      implicit none
          print *,"       -- inside fun_int()"
          fun_int = -8
      end function

      subroutine proc_fsub(x)
      implicit none
         procedure(integer) x
         integer :: y

         print *, "   inside proc_fsub()"
         y = x()
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

         print *, "   inside func_fsub()"
         y = x()
      end  subroutine

