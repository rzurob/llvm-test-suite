! F2008 HYPOT: real(8), non-constant expressions
implicit none
real(8) x, y, z

interface
  pure logical(4) function precision_r8(x, y)
    real(8), intent(in) :: x, y
  end function
end interface

x = -3.0d0
y = -4.0d0
z = hypot(x, y)
if (.not. precision_r8(z, 5.0d0)) error stop 1

x = -0.0d0
y = -0.0d0
z = hypot(x, y)
if (.not. precision_r8(z, 0.0d0)) error stop 2

x = 0.0d0
y = 0.0d0
z = hypot(x, y)
if (.not. precision_r8(z, 0.0d0)) error stop 3

x = 1.0d0
y = 0.0d0
z = hypot(x, y)
if (.not. precision_r8(z, 1.0d0)) error stop 4

x = 10.0d0
y = 6.63324958071080d0
z = hypot(x, y)
if (.not. precision_r8(z, 12.0d0)) error stop 5

x = 20.0d0
y = 20.0d0
z = hypot(x, y)
if (.not. precision_r8(z, 0.282842712474619d2)) error stop 6

x = huge(x)
y = tiny(x)
z = hypot(x, y)
if (.not. precision_r8(z, x)) error stop 7
end
