! F2008 ERFC_SCALED: real(4), constant expressions
implicit none
real(4), parameter :: x(*) = &
  [ -1.0e0, -0.0e0, 0.0e0, 1.0e0, 10.0e0, 20.0e0, 26.0e0 ]
real(4), parameter :: y(*) = erfc_scaled(x)
real(4), parameter :: vf(*) = &
  [ 5.0089801e0,   1.0e0,         1.0e0,        0.42758358e0, &
    0.56140993e-1, 0.28174349e-1, 0.21683585e-1 ]

integer(4) i

interface
  pure logical(4) function precision_r4(x, y)
    real(4), intent(in) :: x, y
  end function
end interface

do i = 1, ubound(x, 1)
  if (.not. precision_r4(y(i), vf(i))) then
    print *, erfc_scaled(x(i))
    print *, y(i)
    print *, vf(i)
    call zzrc(i)
  endif
end do
end
