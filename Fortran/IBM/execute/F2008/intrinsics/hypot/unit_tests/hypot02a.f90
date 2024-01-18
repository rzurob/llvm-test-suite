! F2008 HYPOT: real(8), constant expressions
implicit none
real(8), parameter :: x(*) = &
  [ -3.0d0, -0.0d0, 0.0d0, 1.0d0, 10.0d0, 20.0d0, huge(real(1.0d0, kind=8)) ]
real(8), parameter :: y(*) = &
  [ -4.0d0, -0.0d0, 0.0d0, 0.0d0, 6.63324958071080d0, 20.0d0, tiny(real(1.0d0, kind=8)) ]
real(8), parameter :: z(*) = hypot(x, y)
real(8), parameter :: vf(*) = &
  [ 5.0d0, 0.0d0, 0.0d0, 1.0d0, 12.0d0, 0.282842712474619d2, huge(real(1.0d0, kind=8)) ]

integer(4) i

interface
  pure logical(4) function precision_r8(x, y)
    real(8), intent(in) :: x, y
  end function
end interface

do i = 1, ubound(x, 1)
  if (.not. precision_r8(z(i), vf(i))) then
    print *, hypot(x(i), y(i))
    print *, z(i)
    print *, vf(i)
    call zzrc(i)
  endif
end do
end
