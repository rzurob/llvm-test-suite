!ibm* sourceform(free(f90))
!dir$ free
! Test the new F2008 complex trig intrinsics in non-constant expressions.
!
! This test case is designed to be used with -qrealsize or -qautodbl. It
! compares the result from calling using a promoted type to the result
! from calling a non-promoted type.
! Functions tested: acos, asin, atan,
!                   acosh, asinh, atanh,
!                   cosh, sinh, tanh,
!                   tan
implicit none
complex   :: x(0:9), res1
complex(8):: y(0:9), res2
integer i

x = [ (0.540302d0, 0.000000d0), (0.841471d0, 0.000000d0),  &
      (1.557408d0, 0.000000d0), (1.5430806d0, 0.000000d0), &
      (1.175201d0, 0.000000d0), (0.761594d0, 0.000000d0),  &
      (1.000000d0, 0.000000d0), (0.540302d0, 1.000000d0),  &
      (0.000000d0, 0.841471d0), (0.000000d0, 0.000000d0) ]

y = [ (0.540302d0, 0.000000d0), (0.841471d0, 0.000000d0),  &
      (1.557408d0, 0.000000d0), (1.5430806d0, 0.000000d0), &
      (1.175201d0, 0.000000d0), (0.761594d0, 0.000000d0),  &
      (1.000000d0, 0.000000d0), (0.540302d0, 1.000000d0),  &
      (0.000000d0, 0.841471d0), (0.000000d0, 0.000000d0) ]

do i = 0, 9
  res1 = acos(x(i))
  res2 = acos(y(i))
  if (res1 /= res2) call zzrc(10 + i)

  res1 = asin(x(i))
  res2 = asin(y(i))
  if (res1 /= res2) call zzrc(20 + i)

  res1 = atan(x(i))
  res2 = atan(y(i))
  if (res1 /= res2) call zzrc(30 + i)

  res1 = acosh(x(i))
  res2 = acosh(y(i))
  if (res1 /= res2) call zzrc(40 + i)

  res1 = asinh(x(i))
  res2 = asinh(y(i))
  if (res1 /= res2) call zzrc(50 + i)

  res1 = atanh(x(i))
  res2 = atanh(y(i))
  if (res1 /= res2) call zzrc(60 + i)

  res1 = cosh(x(i))
  res2 = cosh(y(i))
  if (res1 /= res2) call zzrc(70 + i)

  res1 = sinh(x(i))
  res2 = sinh(y(i))
  if (res1 /= res2) call zzrc(80 + i)

  res1 = tanh(x(i))
  res2 = tanh(y(i))
  if (res1 /= res2) call zzrc(90 + i)

  res1 = tan(x(i))
  res2 = tan(y(i))
  if (res1 /= res2) call zzrc(100 + i)
end do
end
