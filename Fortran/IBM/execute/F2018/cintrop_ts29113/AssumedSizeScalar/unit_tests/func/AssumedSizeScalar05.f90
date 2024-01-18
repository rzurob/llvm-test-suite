! Paper 13-283 proposes an interp for TS 291113 (C-interop) that allows
! a scalar actual argument corresponding to an assumed-type and assumed-size
! dummy arg.

  use, intrinsic :: iso_c_binding
  implicit none

  integer(c_int) :: a = 3
  integer(c_int) :: b = 5

  interface
    subroutine c_func_arr_sum(a, b) BIND(c)
      implicit none
      TYPE(*) :: a(*), b(*)
    end subroutine c_func_arr_sum
  end interface

  call c_func_arr_sum(a, b)
  end
