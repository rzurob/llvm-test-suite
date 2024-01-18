! F2008 Implied-shape array
! - character(*) implied-shape arrays
! - Implied-shape array declared inside the BLOCK construct
! - Implied-shape array where the rhs is an array section
! - Implied-shape array where the rhs is zero-sized
character(*), parameter :: c(*) = [ 'abc', 'def' ]
character(*), parameter :: d(3:*) = c(2:1:-1)

if (any(shape(c) /= [ 2 ])) error stop 1
if (lbound(c, 1) /= 1) error stop 2
if (ubound(c, 1) /= 2) error stop 3
if (any(c /= [ 'abc', 'def' ])) error stop 4
if (len(c(1)) /= 3) error stop 5

if (any(shape(d) /= [ 2 ])) error stop 11
if (lbound(d, 1) /= 3) error stop 12
if (ubound(d, 1) /= 4) error stop 13
if (any(d /= [ 'def', 'abc' ])) error stop 14
if (len(d(3)) /= 3) error stop 15

block
  integer, parameter :: c(*, -5:*) = reshape([1, 2, 3, 4, 5], [2, 4], [6, 7, 8])
  type dt
    integer i
  end type
  type(dt), parameter :: d(*) = [dt :: ]
  if (lbound(c, 1) /= 1) error stop 21
  if (lbound(c, 2) /= -5) error stop 22
  if (ubound(c, 1) /= 2) error stop 23
  if (ubound(c, 2) /= -2) error stop 24
  if (any(c(1, :) /= [1, 3, 5, 7])) error stop 25
  if (any(c(2, :) /= [2, 4, 6, 8])) error stop 26

  if (lbound(d, 1) /= 1) error stop 31
  if (ubound(d, 1) /= 0) error stop 32
  if (size(d, 1) /= 0) error stop 33
end block
end
