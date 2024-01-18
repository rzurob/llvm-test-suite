! F2008 ERFC_SCALED: real(16), non-constant expressions
implicit none
real(16) x, y

interface
  pure logical(4) function precision_r16(x, y)
    real(16), intent(in) :: x, y
  end function
end interface

x = -1.0q0
y = erfc_scaled(x)
if (.not. precision_r16(y, 5.008980080762283466309824598215q0)) error stop 1

x = -0.0q0
y = erfc_scaled(x)
if (.not. precision_r16(y, 1.0q0)) error stop 2

x = 0.0q0
y = erfc_scaled(x)
if (.not. precision_r16(y, 1.0q0)) error stop 3

x = 1.0q0
y = erfc_scaled(x)
if (.not. precision_r16(y, 0.4275835761558070044107503444905q0)) error stop 4

x = 10.0q0
y = erfc_scaled(x)
if (.not. precision_r16(y, 0.5614099274382258585751738722047q-1)) error stop 5

x = 20.0q0
y = erfc_scaled(x)
if (.not. precision_r16(y, 0.2817434874105131931864915453447q-1)) error stop 6

x = 26.0q0
y = erfc_scaled(x)
if (.not. precision_r16(y, 0.21683584850562906616172991687293390q-01)) error stop 7
end
