! Tests that generic bindings can successfully appear as selectors

module m
  type dt
    integer i
  contains
    generic :: operator(*) => add, add2
    procedure :: add
    procedure, pass(b) :: add2 => add
  end type
contains
  type(dt) function add(a,b)
    class(dt), intent(in) :: a, b
    add%i = a%i * b%i * 10
  end function
end module

use m
type(dt) :: x(2) = (/ dt(5), dt(2) /)
class(dt), allocatable :: g(:)

! non-polymorphic case
associate (aname => x(1) * dt(4) * x(2))
  if (aname%i /= 4000) then
    print *, aname%i
    error stop 1_4
  endif
end associate

! polymorphic case
allocate(g(2), source = x)
associate (aname => x(1) * dt(2) * x(2))
  if (aname%i /= 2000) then
    print *, aname%i
    error stop 2_4
  endif
end associate
end
