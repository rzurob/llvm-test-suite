! Pointer initialization
! parameters (constants) with pointer initialization
module m
  implicit none
  integer, target :: t
  type dt
    integer i
    integer, pointer :: p => t
  end type
  type(dt), parameter :: d = dt(3)
end module

program main
  use m
  implicit none
  t = 5
  if (d%i /= 3) error stop 1
  if (.not. associated(d%p, t)) error stop 2
  if (d%p /= t) error stop 3
end program
