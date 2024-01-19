use c_trig
implicit none
integer, parameter :: complex_kind = 4
integer, parameter :: num_tests = 10
complex(complex_kind), parameter :: x(num_tests) =       &
  [                                                      &
    (0.540302e0, 0.000000e0), (0.841471e0, 0.000000e0),  &
    (1.557408e0, 0.000000e0), (1.5430806e0, 0.000000e0), &
    (1.175201e0, 0.000000e0), (0.761594e0, 0.000000e0),  &
    (1.000000e0, 0.000000e0), (1.000000e0, 0.000000e0),  &
    (1.000000e0, 0.000000e0), (1.000000e0, 0.000000e0)   &
  ]

complex(complex_kind), parameter :: res(num_tests) =     &
  [                                                      &
    acos(x(1)),  asin(x(2)),  atan(x(3)),                &
    acosh(x(4)), asinh(x(5)), atanh(x(6)),               &
    cosh(x(7)),  sinh(x(8)),  tanh(x(9)),                &
    tan(x(10))                                           &
  ]

complex(complex_kind) c_res(num_tests)

integer n

n = 1
call c_acos(x(n), c_res(n))
call check(res(n), c_res(n), n)

n = 2
call c_asin(x(n), c_res(n))
call check(res(n), c_res(n), n)

n = 3
call c_atan(x(n), c_res(n))
call check(res(n), c_res(n), n)

n = 4
call c_acosh(x(n), c_res(n))
call check(res(n), c_res(n), n)

n = 5
call c_asinh(x(n), c_res(n))
call check(res(n), c_res(n), n)

n = 6
call c_atanh(x(n), c_res(n))
call check(res(n), c_res(n), n)

n = 7
call c_cosh(x(n), c_res(n))
call check(res(n), c_res(n), n)

n = 8
call c_sinh(x(n), c_res(n))
call check(res(n), c_res(n), n)

n = 9
call c_tanh(x(n), c_res(n))
call check(res(n), c_res(n), n)

n = 10
call c_tan(x(n), c_res(n))
call check(res(n), c_res(n), n)

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
