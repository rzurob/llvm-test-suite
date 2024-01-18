! Polymorphic recursive allocatable type, initialized 3 levels deep
implicit none
type dt
  integer i
  class(dt), allocatable :: p
end type
type, extends(dt) :: et
  integer j
end type
type(dt) x

x = dt(5, et(6, dt(7, null()), -6))
if (x%i /= 5) error stop 1
if (.not. allocated(x%p)) error stop 2
select type (xp => x%p)
type is (et)
  if (xp%i /= 6) error stop 3
  if (.not. allocated(xp%p)) error stop 4
  select type (xpp => xp%p)
  type is (dt)
    if (xp%p%i /= 7) error stop 5
    if (allocated(xp%p%p)) error stop 6

  class default
    error stop 7
  end select
  if (xp%j /= -6) error stop 8

class default
  error stop 9
end select

deallocate(x%p)
if (allocated(x%p)) error stop 10

allocate(x%p, source=dt(50, null()))
if (.not. allocated(x%p)) error stop 11
if (x%p%i /= 50) error stop 12
if (allocated(x%p%p)) error stop 13

end
