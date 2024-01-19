implicit none
real(4) :: x, y, z
real(8) x8
interface
  logical function precision_r4(x, y)
    real(4) x, y
  end function
end interface

x = -9.25e0
do while (x <= 10.0e0)
  x8 = x
  y = exp(x8 * x8) * erfc(x8)
  z = erfc_scaled(x)
  if (.not. precision_r4(y, z)) then
    print *, 'x=', x
    print *, 'y=', y
    print *, 'z=', z
    error stop 1
  end if
  x = x + 0.05e0
end do
end
