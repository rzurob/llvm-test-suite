!ibm* sourceform(free(f90))
!dir$ free
! Test the new F2008 complex trig intrinsics in non-constant expressions.
! The strategy here is to call the same intrinsic in Fortran and in C and
! ensure that the results are identical.  (Calling in C resolved to libm).
! Functions tested: acos, asin, atan,
!                   acosh, asinh, atanh,
!                   cosh, sinh, tanh,
!                   tan
use c_trig
implicit none
integer, parameter :: complex_kind = 16
complex(complex_kind) x(0:9), res, c_res
integer i

x = [ (0.540302q0, 0.000000q0), (0.841471q0, 0.000000q0),  &
      (1.557408q0, 0.000000q0), (1.5430806q0, 0.000000q0), &
      (1.175201q0, 0.000000q0), (0.761594q0, 0.000000q0),  &
      (1.000000q0, 0.000000q0), (0.540302q0, 1.000000q0),  &
      (0.000000q0, 0.841471q0), (0.000000q0, 0.000000q0) ]

do i = 0, 9
  res = acos(x(i))
  call c_acos(x(i), c_res)
  call check(res, c_res, 10 + i)

  res = asin(x(i))
  call c_asin(x(i), c_res)
  call check(res, c_res, 20 + i)

  res = atan(x(i))
  call c_atan(x(i), c_res)
  call check(res, c_res, 30 + i)

  res = acosh(x(i))
  call c_acosh(x(i), c_res)
  call check(res, c_res, 40 + i)

  res = asinh(x(i))
  call c_asinh(x(i), c_res)
  call check(res, c_res, 50 + i)

  res = atanh(x(i))
  call c_atanh(x(i), c_res)
  call check(res, c_res, 60 + i)

  res = cosh(x(i))
  call c_cosh(x(i), c_res)
  call check(res, c_res, 70 + i)

  res = sinh(x(i))
  call c_sinh(x(i), c_res)
  call check(res, c_res, 80 + i)

  res = tanh(x(i))
  call c_tanh(x(i), c_res)
  call check(res, c_res, 90 + i)

  res = tan(x(i))
  call c_tan(x(i), c_res)
  call check(res, c_res, 100 + i)
end do

contains
  subroutine check(a, b, rc)
    complex(complex_kind), value :: a, b
    integer(4), value :: rc

    if (a /= b .and. .not. are_equal(a, b)) then
      print *, a
      print *, b
      call zzrc(rc)
    endif
  end subroutine
end
