! F2008 ERFC_SCALED: real(16), constant expressions
implicit none
real(16), parameter :: x(*) = &
  [ -1.0q0, -0.0q0, 0.0q0, 1.0q0, 10.0q0, 20.0q0, 26.0q0 ]
real(16), parameter :: y(*) = erfc_scaled(x)
real(16), parameter :: vf(*) =            &
  [ 5.008980080762283466309824598215q0,   &
    1.0q0,                                &
    1.0q0,                                &
    0.4275835761558070044107503444905q0,  &
    0.5614099274382258585751738722047q-1, &
    0.2817434874105131931864915453447q-1, &
    0.21683584850562906616172991687293390q-01 ]
    
integer(4) i

interface
  pure logical(4) function precision_r16(x, y)
    real(16), intent(in) :: x, y
  end function
end interface

do i = 1, ubound(x, 1)
  if (.not. precision_r16(y(i), vf(i))) then
    print *, erfc_scaled(x(i))
    print *, y(i)
    print *, vf(i)
    call zzrc(i)
  endif
end do
end
