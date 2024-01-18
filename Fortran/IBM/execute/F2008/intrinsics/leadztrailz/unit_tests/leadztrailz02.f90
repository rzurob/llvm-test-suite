! F2008 leadz and trailz intrinsics in constant expressions
implicit none
integer(1), parameter :: i1(8) = [z'ff', z'f0', z'0f', z'00', z'04', z'40', z'80', z'01']
integer(2), parameter :: i2(8) = [z'ffff', z'ff00', z'00ff', z'0000', &
                                  z'0f0f', z'f00f', z'8000', z'0001']
integer(4), parameter :: i4(8) = [z'ffffffff', z'ffff0000',                 &
                                  z'0000ffff', z'00000000',                 &
                                  z'0f0f0f0f', z'ff0000ff',                 &
                                  z'80000000', z'00000001']
integer(8), parameter :: i8(8) = [z'ffffffffffffffff', z'ffffffff00000000', &
                                  z'00000000ffffffff', z'0000000000000000', &
                                  z'0f0f0f0f0f0f0f0f', z'ffff00000000ffff', &
                                  z'8000000000000000', z'0000000000000001']

integer(1), parameter :: leadz_result1(8) = leadz(i1)
integer(1), parameter :: trailz_result1(8) = trailz(i1)

integer(2), parameter :: leadz_result2(8) = leadz(i2)
integer(2), parameter :: trailz_result2(8) = trailz(i2)

integer(4), parameter :: leadz_result4(8) = leadz(i4)
integer(4), parameter :: trailz_result4(8) = trailz(i4)

integer(8), parameter :: leadz_result8(8) = leadz(i8)
integer(8), parameter :: trailz_result8(8) = trailz(i8)

integer i

do i = 1, ubound(i1, 1)
  print '(" ",b64.8," ",i2," ",i2)', i1(i), leadz_result1(i), trailz_result1(i)
end do

do i = 1, ubound(i2, 1)
  print '(" ",b64.16," ",i2," ",i2)', i2(i), leadz_result2(i), trailz_result2(i)
end do

do i = 1, ubound(i4, 1)
  print '(" ",b64.32," ",i2," ",i2)', i4(i), leadz_result4(i), trailz_result4(i)
end do

do i = 1, ubound(i8, 1)
  print '(" ",b64.64," ",i2," ",i2)', i8(i), leadz_result8(i), trailz_result8(i)
end do
end
