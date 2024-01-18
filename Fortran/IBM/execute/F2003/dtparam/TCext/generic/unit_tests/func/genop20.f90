! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/unit_tests/func/genop20.f
! opt variations: -qnol -qnodeferredlp

! Tests that generic bindings can successfully appear as selectors

module m
  type dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)      i
  contains
    generic :: operator(*) => add, add2
    procedure :: add
    procedure, pass(b) :: add2 => add
  end type
contains
  type(dt(20,4)) function add(a,b)
    class(dt(*,4)), intent(in) :: a, b
    add%i = a%i * b%i * 10
  end function
end module

use m
type(dt(20,4)) :: x(2) = (/ dt(20,4)(5), dt(20,4)(2) /)
class(dt(:,4)), allocatable :: g(:)

! non-polymorphic case
associate (aname => x(1) * dt(20,4)(4) * x(2))
  if (aname%i /= 4000) then
    print *, aname%i
    error stop 1_4
  endif
end associate

! polymorphic case
allocate(g(2), source = x)
associate (aname => x(1) * dt(20,4)(2) * x(2))
  if (aname%i /= 2000) then
    print *, aname%i
    error stop 2_4
  endif
end associate
end
