! Recursive allocatable type, initialized 3 levels deep
implicit none
type dt
  integer i
  type(dt), allocatable :: p
end type
type(dt) x

x = dt(5, dt(6, dt(7, null())))
if (x%i /= 5) error stop 1
if (.not. allocated(x%p)) error stop 2
if (x%p%i /= 6) error stop 3
if (.not. allocated(x%p%p)) error stop 4
if (x%p%p%i /= 7) error stop 5
if (allocated(x%p%p%p)) error stop 6

deallocate(x%p)
if (allocated(x%p)) error stop 7

allocate(x%p, source=dt(50, null()))
if (.not. allocated(x%p)) error stop 8
if (x%p%i /= 50) error stop 9
if (allocated(x%p%p)) error stop 10

end
