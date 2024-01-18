program generic_allocatable_pointer_f100
implicit none

   interface proc_data_sub
      subroutine proc_sub(x)
!         procedure(integer) :: x
         procedure() :: x
      end  subroutine

      subroutine data_sub(x)
         integer :: x
      end  subroutine
   end interface

   interface
      integer function fun1()
      end function
   end interface

     integer               :: i_res /5/

      call proc_data_sub(i_res)
      call proc_data_sub(fun1)
end program

      integer function fun1()
      implicit none
          fun1 = int(0.5)
      end function

      subroutine proc_sub(x)
      implicit none
!         procedure(integer) x
         procedure() x

         print *, "   inside proc_sub()"
      end  subroutine

      subroutine data_sub(x)
      implicit none
         integer :: x
         x = x * 7
         print *, "   inside data_sub()  x=",x
      end  subroutine
