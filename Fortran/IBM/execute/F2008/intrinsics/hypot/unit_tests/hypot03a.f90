! F2008 HYPOT: real(16), constant expressions
implicit none
real(16), parameter :: x(*) = &
  [ -3.0q0,                              &
    -0.0q0,                              &
     0.0q0,                              &
     1.0q0,                              &
    10.0q0,                              &
    20.0q0 ]
real(16), parameter :: y(*) = &
  [ -4.0q0,                              &
    -0.0q0,                              &
     0.0q0,                              &
     0.0q0,                              &
     6.633249580710799698229865473341q0, &
     19.59591794226542478557827259765q0 ]
real(16), parameter :: z(*) = hypot(x, y)
real(16), parameter :: vf(*) =           &
  [ 5.0q0,                               &
    0.0q0,                               &
    0.0q0,                               &
    1.0q0,                               &
    12.0q0,                              &
    28.0q0 ]
    
integer(4) i

interface
  pure logical(4) function precision_r16(x, y)
    real(16), intent(in) :: x, y
  end function
end interface

do i = 1, ubound(x, 1)
  if (.not. precision_r16(z(i), vf(i))) then
    print *, hypot(x(i), y(i))
    print *, z(i)
    print *, vf(i)
    call zzrc(i)
  endif
end do
end
