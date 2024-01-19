! F2008 HYPOT: real(4), non-constant expressions
implicit none
real(4) x, y, z

interface
  pure logical(4) function precision_r4(x, y)
    real(4), intent(in) :: x, y
  end function
end interface

x = -3.0e0
y = -4.0e0
z = hypot(x, y)
if (.not. precision_r4(z, 5.0e0)) error stop 1

x = -0.0e0
y = -0.0e0
z = hypot(x, y)
if (.not. precision_r4(z, 0.0e0)) error stop 2

x = 0.0e0
y = 0.0e0
z = hypot(x, y)
if (.not. precision_r4(z, 0.0e0)) error stop 3

x = 1.0e0
y = 0.0e0
z = hypot(x, y)
if (.not. precision_r4(z, 1.0e0)) error stop 4

x = 10.0e0
y = 6.63324958e0
z = hypot(x, y)
if (.not. precision_r4(z, 12.0e0)) error stop 5

x = 20.0e0
y = 20.0e0
z = hypot(x, y)
if (.not. precision_r4(z, 0.28284271e2)) error stop 6

x = huge(x)
y = tiny(x)
z = hypot(x, y)
if (.not. precision_r4(z, x)) error stop 7
end
