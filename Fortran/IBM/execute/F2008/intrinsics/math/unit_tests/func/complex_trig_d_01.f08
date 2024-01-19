!ibm* sourceform(free(f90))
!dir$ free
! Test the new F2008 complex trig intrinsics in non-constant expressions.
! The strategy here is to call the same intrinsic in Fortran and in C and
! ensure that the results are identical.  (Calling in C resolved to libm).
! Note that I'm not using the precision_r* functions because the Fortran
! implementation is calling libm.  Consequently, this unit test expects
! identical (bit for bit) results.  If we change the implementation,
! only one place in check() below needs to change to call precision_r*.
! Functions tested: acos, asin, atan,
!                   acosh, asinh, atanh,
!                   cosh, sinh, tanh,
!                   tan
use c_trig
implicit none
integer, parameter :: complex_kind = 8
complex(complex_kind) x(0:9), res, c_res
integer i

x = [ (0.540302d0, 0.000000d0), (0.841471d0, 0.000000d0),  &
      (1.557408d0, 0.000000d0), (1.5430806d0, 0.000000d0), &
      (1.175201d0, 0.000000d0), (0.761594d0, 0.000000d0),  &
      (1.000000d0, 0.000000d0), (0.540302d0, 1.000000d0),  &
      (0.000000d0, 0.841471d0), (0.000000d0, 0.000000d0) ]

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

    if (a /= b) then
      print *, a
      print *, b
      call zzrc(rc)
    endif
  end subroutine
end
