! Structure constructor that elides allocatable component initialization.
! Parameters.
module m
  implicit none
  type dt
    integer i
    integer, allocatable :: q
    type(dt), allocatable :: p
    type(dt), allocatable :: r
  end type
  type(dt), parameter :: y1 = dt(1, null(), null(), null())
  type(dt), parameter :: y2 = dt(2, null())
  type(dt), parameter :: y3 = dt(3, null(), p=null())
end module

program main
  use m
  implicit none

  if (y1%i /= 1) error stop 11
  if (allocated(y1%q)) error stop 12
  if (allocated(y1%p)) error stop 13
  if (allocated(y1%r)) error stop 14

  if (y2%i /= 2) error stop 21
  if (allocated(y2%q)) error stop 22
  if (allocated(y2%p)) error stop 23
  if (allocated(y2%r)) error stop 24

  if (y3%i /= 3) error stop 31
  if (allocated(y3%q)) error stop 32
  if (allocated(y3%p)) error stop 33
  if (allocated(y3%r)) error stop 34
end
