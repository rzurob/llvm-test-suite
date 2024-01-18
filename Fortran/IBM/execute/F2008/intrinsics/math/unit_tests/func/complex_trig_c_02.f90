!ibm* sourceform(free(f90))
!dir$ free
! Test the new F2008 complex trig intrinsics in constant expressions.
!
! This test case is designed to be used with -qrealsize or -qautodbl=dbl[pad]4.
! It compares the result from calling using a promoted type to the result
! from calling a non-promoted type.
! Functions tested: acos, asin, atan,
!                   acosh, asinh, atanh,
!                   cosh, sinh, tanh,
!                   tan
implicit none
integer, parameter :: num_tests = 10
complex, parameter :: x(num_tests) =                     &
  [                                                      &
    (0.540302d0, 0.000000d0), (0.841471d0, 0.000000d0),  &
    (1.557408d0, 0.000000d0), (1.5430806d0, 0.000000d0), &
    (1.175201d0, 0.000000d0), (0.761594d0, 0.000000d0),  &
    (1.000000d0, 0.000000d0), (1.000000d0, 0.000000d0),  &
    (1.000000d0, 0.000000d0), (1.000000d0, 0.000000d0)   &
  ]

complex, parameter :: res1(num_tests) =                  &
  [                                                      &
    acos(x(1)),  asin(x(2)),  atan(x(3)),                &
    acosh(x(4)), asinh(x(5)), atanh(x(6)),               &
    cosh(x(7)),  sinh(x(8)),  tanh(x(9)),                &
    tan(x(10))                                           &
  ]

complex(8), parameter :: res2(num_tests) =               &
  [                                                      &
    acos(x(1)),  asin(x(2)),  atan(x(3)),                &
    acosh(x(4)), asinh(x(5)), atanh(x(6)),               &
    cosh(x(7)),  sinh(x(8)),  tanh(x(9)),                &
    tan(x(10))                                           &
  ]

integer(4) i
  
do i = 1, num_tests
  if (res1(i) /= res2(i)) call zzrc(i)
end do
end
