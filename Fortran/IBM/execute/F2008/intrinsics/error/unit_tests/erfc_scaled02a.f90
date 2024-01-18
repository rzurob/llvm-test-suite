! F2008 ERFC_SCALED: real(8), constant expressions
implicit none
real(8), parameter :: x(*) = &
  [ -1.0d0, -0.0d0, 0.0d0, 1.0d0, 10.0d0, 20.0d0, 26.0d0 ]
real(8), parameter :: y(*) = erfc_scaled(x)
real(8), parameter :: vf(*) = &
  [ 5.00898008076228d0,   1.0d0,                1.0d0,  &
    0.427583576155807d0,  0.561409927438226d-1, 0.281743487410513d-1, &
    0.216835848505629d-1 ]

integer(4) i

interface
  pure logical(4) function precision_r8(x, y)
    real(8), intent(in) :: x, y
  end function
end interface

do i = 1, ubound(x, 1)
  if (.not. precision_r8(y(i), vf(i))) then
    print *, erfc_scaled(x(i))
    print *, y(i)
    print *, vf(i)
    call zzrc(i)
  endif
end do
end
