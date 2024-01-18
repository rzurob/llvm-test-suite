! F2008 Implied-shape arrays
! - rhs is ac-implied-do
! - rhs is ac containing an implied-do
! - lbound, ubound, size of implied-shape array used in a constant
!   expression
implicit none
integer i
integer, parameter :: x(*) = [ (i*3, i=1, 10) ]
integer, parameter :: y(11:*) = [ (7, i=1, 5), x ]

integer, parameter :: z(*) = [ lbound(x, 1), ubound(x, 1), size(x, 1), &
                               lbound(y, 1), ubound(y, 1), size(y, 1) ]

if (any(z /= [1, 10, 10, 11, 25, 15])) error stop 1
end
