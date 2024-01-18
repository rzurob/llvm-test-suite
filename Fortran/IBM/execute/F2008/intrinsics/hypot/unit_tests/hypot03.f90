! F2008 HYPOT: real(16), non-constant expressions
implicit none
real(16) x, y, z

interface
  pure logical(4) function precision_r16(x, y)
    real(16), intent(in) :: x, y
  end function
end interface

x = -3.0q0
y = -4.0q0
z = hypot(x, y)
if (.not. precision_r16(z, 5.0q0)) error stop 1

x = -0.0q0
y = -0.0q0
z = hypot(x, y)
if (.not. precision_r16(z, 0.0q0)) error stop 2

x = 0.0q0
y = 0.0q0
z = hypot(x, y)
if (.not. precision_r16(z, 0.0q0)) error stop 3

x = 1.0q0
y = 0.0q0
z = hypot(x, y)
if (.not. precision_r16(z, 1.0q0)) error stop 4

x = 10.0q0
y = 6.633249580710799698229865473341q0
z = hypot(x, y)
if (.not. precision_r16(z, 12.0q0)) error stop 5

x = 20.0q0
y = 19.59591794226542478557827259765q0
z = hypot(x, y)
if (.not. precision_r16(z, 28.0q0)) error stop 6

end
