! Structure constructor that elides allocatable component initialization.
implicit none
type dt
  integer i
  integer, allocatable :: q
  type(dt), allocatable :: p
  type(dt), allocatable :: r
end type
type(dt) x

x = dt(5, null())
if (x%i /= 5) error stop 1
if (allocated(x%q)) error stop 2
if (allocated(x%p)) error stop 3
if (allocated(x%r)) error stop 4

x = dt(6, p=dt(3))
if (x%i /= 6) error stop 11
if (allocated(x%q)) error stop 12
if (.not. allocated(x%p)) error stop 13
if (x%p%i /= 3) error stop 14
if (allocated(x%p%p)) error stop 15
if (allocated(x%r)) error stop 16

x = dt(7, null(), p=null())
if (x%i /= 7) error stop 21
if (allocated(x%q)) error stop 22
if (allocated(x%p)) error stop 23
if (allocated(x%r)) error stop 24

end
