! F2008 implied-shape arrays
module m
  implicit none
  integer, parameter :: x(*) = [2, 3]
  complex, parameter :: y(3:*) = [ (-1.0, -2.0), (-3.0, -4.0), (-5.0, -6.0) ]
  complex :: z
  dimension :: z(*, 2:*)
  parameter (z = reshape([ (1.0, 2.0), (3.0, 4.0), (5.0, 6.0), y ], &
                         [ 3, 2 ]))
end module m

use m
implicit none

if (lbound(x, 1) /= 1) error stop 1
if (ubound(x, 1) /= 2) error stop 2
if (size(x, 1) /= 2) error stop 3
if (x(1) /= 2) error stop 4
if (x(2) /= 3) error stop 5

if (lbound(y, 1) /= 3) error stop 11
if (ubound(y, 1) /= 5) error stop 12
if (size(y, 1) /= 3) error stop 13

if (lbound(z, 1) /= 1) error stop 21
if (lbound(z, 2) /= 2) error stop 22
if (ubound(z, 1) /= 3) error stop 23
if (ubound(z, 2) /= 3) error stop 24
if (size(z, 1) /= 3) error stop 25
if (size(z, 2) /= 2) error stop 26
if (z(3, 2) /= (5.0, 6.0)) error stop 27
if (z(3, 3) /= (-5.0, -6.0)) error stop 28

end
