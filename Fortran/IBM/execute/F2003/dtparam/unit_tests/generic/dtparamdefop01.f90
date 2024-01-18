! Simple defined + operator with kind type parameters.
module m
  type dt(k)
    integer, kind :: k
    integer(k) i
  contains
    procedure :: add => f1
    generic :: operator(+) => add
  end type
contains
  integer function f1(a,b)
    class(dt(4)), intent(in) :: a,b
    f1 = a%i + b%i + 1
  end function
end module

use m
type(dt(4)) x, y
integer i
x%i = 4
y%i = 3
i = x + y
if (i /= 8) then
  print *, i
  error stop 1
endif
end
