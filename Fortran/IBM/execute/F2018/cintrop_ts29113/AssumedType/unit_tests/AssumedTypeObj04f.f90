!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr 14, 2012
!*                             : IBM Software Solutions China Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop Assumed-type object
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Calling a BIND(C) procedure from Fortran
!*                               where the procedure is defined in C with
!*                               assumed-type object.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program AssumedTypeObj04f
  use, intrinsic :: iso_c_binding
  implicit none

  integer(c_int) :: a = 3
  integer(c_int) :: b = 5
  integer(c_int) :: arr1(3) = 3
  integer(c_int) :: arr2(3) = 4

  interface
    subroutine c_func_add(a, b) BIND(c)
      implicit none

      TYPE(*) :: a, b
    end subroutine c_func_add
    subroutine c_func_arr_sum(a, b, len) BIND(c)
      implicit none

      TYPE(*) :: a(*), b(*), len
    end subroutine c_func_arr_sum
  end interface

  call sub(a, b)
  call arr_add(arr1, arr2, a)

  contains
    subroutine sub(a, b)
      TYPE(*) :: a
      TYPE(*) :: b

      call c_func_add(a, b)
    end

    subroutine arr_add(a, b, len)
      TYPE(*) :: a(*)
      TYPE(*) :: b(*)
      TYPE(*) :: len

      call c_func_arr_sum(a, b, len)
    end
end
