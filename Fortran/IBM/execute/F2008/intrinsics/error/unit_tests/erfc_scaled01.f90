! F2008 ERFC_SCALED: real(4), non-constant expressions
implicit none
real(4) x, y

interface
  pure logical(4) function precision_r4(x, y)
    real(4), intent(in) :: x, y
  end function
end interface

x = -1.0e0
y = erfc_scaled(x)
if (.not. precision_r4(y, 5.0089801e0)) error stop 1

x = -0.0e0
y = erfc_scaled(x)
if (.not. precision_r4(y, 1.0e0)) error stop 2

x = 0.0e0
y = erfc_scaled(x)
if (.not. precision_r4(y, 1.0e0)) error stop 3

x = 1.0e0
y = erfc_scaled(x)
if (.not. precision_r4(y, 0.42758358e0)) error stop 4

x = 10.0e0
y = erfc_scaled(x)
if (.not. precision_r4(y, 0.56140993e-1)) error stop 5

x = 20.0e0
y = erfc_scaled(x)
if (.not. precision_r4(y, 0.28174349e-1)) error stop 6

x = 26.0e0
y = erfc_scaled(x)
if (.not. precision_r4(y, 0.21683585e-1)) error stop 7
end
