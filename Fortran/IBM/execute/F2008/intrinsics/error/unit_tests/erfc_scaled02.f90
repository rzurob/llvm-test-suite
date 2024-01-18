! F2008 ERFC_SCALED: real(8), non-constant expressions
implicit none
real(8) x, y

interface
  pure logical(4) function precision_r8(x, y)
    real(8), intent(in) :: x, y
  end function
end interface

x = -1.0d0
y = erfc_scaled(x)
if (.not. precision_r8(y, 5.00898008076228d0)) error stop 1

x = -0.0d0
y = erfc_scaled(x)
if (.not. precision_r8(y, 1.0d0)) error stop 2

x = 0.0d0
y = erfc_scaled(x)
if (.not. precision_r8(y, 1.0d0)) error stop 3

x = 1.0d0
y = erfc_scaled(x)
if (.not. precision_r8(y, 0.427583576155807d0)) error stop 4

x = 10.0d0
y = erfc_scaled(x)
if (.not. precision_r8(y, 0.561409927438226d-1)) error stop 5

x = 20.0d0
y = erfc_scaled(x)
if (.not. precision_r8(y, 0.281743487410513d-1)) error stop 6

x = 26.0d0
y = erfc_scaled(x)
if (.not. precision_r8(y, 0.216835848505629d-1)) error stop 7
end
