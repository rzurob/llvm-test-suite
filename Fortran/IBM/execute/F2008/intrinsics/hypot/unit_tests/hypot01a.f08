! F2008 HYPOT: real(4), constant expressions
implicit none
real(4), parameter :: x(*) = &
  [ -3.0e0, -0.0e0, 0.0e0, 1.0e0, 10.0e0, 20.0e0, huge(real(1.0, kind=4)) ]
real(4), parameter :: y(*) = &
  [ -4.0e0, -0.0e0, 0.0e0, 0.0e0, 6.63324958e0, 20.0e0, tiny(real(1.0, kind=4)) ]
real(4), parameter :: z(*) = hypot(x, y)
real(4), parameter :: vf(*) = &
  [ 5.0e0, 0.0e0, 0.0e0, 1.0e0, 12.0e0, 0.28284271e2, huge(real(1.0, kind=4)) ]

integer(4) i

interface
  pure logical(4) function precision_r4(x, y)
    real(4), intent(in) :: x, y
  end function
end interface

do i = 1, ubound(x, 1)
  if (.not. precision_r4(z(i), vf(i))) then
    print *, hypot(x(i), y(i))
    print *, z(i)
    print *, vf(i)
    call zzrc(i)
  endif
end do
end
