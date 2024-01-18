!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : PassInterProcPtrDiag.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Ren Jian Gang
!*  DATE                       : March 06, 2011
!*  ORIGIN                     : Compiler Development, IBM CDL
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 303977
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Now Fortran 2008 allows internal procedures and pointers to such procedures
!*  to be actual arguments.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  contains
    subroutine test_pass_inter_proc1(print)
      interface
        subroutine print()
        end subroutine
      end interface

      call print()
    end

    subroutine test_pass_inter_proc2(print)
      interface
        integer function print()
        end function
      end interface

      print *, "print2:", print()
    end

    subroutine test_pass_inter_proc3(print)
      interface
        subroutine print(arg)
          integer, intent(in) :: arg
        end subroutine
      end interface

      call print(345)
    end

    subroutine test_pass_inter_proc4(print)
      interface
        real function print(arg1, arg2)
          real, intent(in) :: arg1, arg2
        end function
      end interface

      real :: i = 2.0
      real :: j = 4.0

      print "(a, f4.1)", " print4: ", print(i, j)
    end

    subroutine test_pass_inter_proc5(print)
      interface
        subroutine print(arg1, arg2, arg3, arg4, arg5)
          integer, intent(in) :: arg1
          real, intent(in) :: arg2
          complex, intent(in) :: arg3
          character(*), intent(in) :: arg4
          logical, intent(in) :: arg5
        end subroutine
      end interface

      real :: r = 3.0

      call print(1 + 2, r * r - r, (2, 3), "hello world", .TRUE.)
    end
end module

program PassInterProcPtrDiag
  use m

  interface
    subroutine proc1()
    end subroutine

    integer function proc2()
    end function

    subroutine proc3(arg)
      integer, intent(in) :: arg
    end subroutine

    real function proc4(arg1, arg2)
      real, intent(in) :: arg1, arg2
    end function

    subroutine proc5(arg1, arg2, arg3, arg4, arg5)
      integer, intent(in) :: arg1
      real, intent(in) :: arg2
      complex, intent(in) :: arg3
      character(*), intent(in) :: arg4
      logical, intent(in) :: arg5
    end subroutine
  end interface

  procedure(proc1), pointer :: p1
  procedure(proc2), pointer :: p2
  procedure(proc3), pointer :: p3
  procedure(proc4), pointer :: p4
  procedure(proc5), pointer :: p5

  p1=>print1
  p2=>print2
  p3=>print3
  p4=>print4
  p5=>print5

  call test_pass_inter_proc1(p1)
  call test_pass_inter_proc2(p2)
  call test_pass_inter_proc3(p3)
  call test_pass_inter_proc4(p4)
  call test_pass_inter_proc5(p5)

  contains
    subroutine print1() ! subroutine-no arg
      print *, "print1: hello world"
    end

    integer function print2() ! function-no arg
      print2 = 9999
    end

    subroutine print3(arg) ! subroutine-one arg
      integer, intent(in) :: arg

      print *, "print3: arg =", arg
    end

    real function print4(arg1, arg2) ! function-two args
      real, intent(in) :: arg1, arg2

      print4 = arg1 + arg2 * 2
    end function

    subroutine print5(arg1, arg2, arg3, arg4, arg5) ! subroutine-intrinisc args
      integer, intent(in) :: arg1
      real, intent(in) :: arg2
      complex, intent(in) :: arg3
      character(*), intent(in) :: arg4
      logical, intent(in) :: arg5

      print *, "print5: integer =", arg1
      print "(a, f3.1)", " print5: real = ", arg2
      print "(a, f3.1, f3.1)", " print5: complex = ", arg3
      print *, "print5: characters = ", trim(arg4)
      print *, "print5: logical =", arg5
    end subroutine
end
