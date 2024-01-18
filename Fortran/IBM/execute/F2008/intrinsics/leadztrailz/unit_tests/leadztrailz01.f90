! F2008 leadz and trailz intrinsics in non-constant expressions
implicit none
integer(1) :: i1(8) = [z'ff', z'f0', z'0f', z'00', z'04', z'40', z'80', z'01']
integer(2) :: i2(8) = [z'ffff', z'ff00', z'00ff', z'0000', &
                       z'0f0f', z'f00f', z'8000', z'0001']
integer(4) :: i4(8) = [z'ffffffff', z'ffff0000',                 &
                       z'0000ffff', z'00000000',                 &
                       z'0f0f0f0f', z'ff0000ff',                 &
                       z'80000000', z'00000001']
integer(8) :: i8(8) = [z'ffffffffffffffff', z'ffffffff00000000', &
                       z'00000000ffffffff', z'0000000000000000', &
                       z'0f0f0f0f0f0f0f0f', z'ffff00000000ffff', &
                       z'8000000000000000', z'0000000000000001']

integer leadz_result, trailz_result
integer i

do i = 1, ubound(i1, 1)
  leadz_result = leadz(i1(i))
  trailz_result = trailz(i1(i))
  print '(" ",b64.8," ",i2," ",i2)', i1(i), leadz_result, trailz_result
end do

do i = 1, ubound(i2, 1)
  leadz_result = leadz(i2(i))
  trailz_result = trailz(i2(i))
  print '(" ",b64.16," ",i2," ",i2)', i2(i), leadz_result, trailz_result
end do

do i = 1, ubound(i4, 1)
  leadz_result = leadz(i4(i))
  trailz_result = trailz(i4(i))
  print '(" ",b64.32," ",i2," ",i2)', i4(i), leadz_result, trailz_result
end do

do i = 1, ubound(i8, 1)
  leadz_result = leadz(i8(i))
  trailz_result = trailz(i8(i))
  print '(" ",b64.64," ",i2," ",i2)', i8(i), leadz_result, trailz_result
end do
end
