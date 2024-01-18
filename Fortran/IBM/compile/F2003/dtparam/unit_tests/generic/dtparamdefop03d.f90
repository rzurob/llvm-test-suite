! Defined operator diagnostic: Kind type parameter can't be assumed
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
    class(dt(*)), intent(in) :: a,b
    f1 = a%i + b%i
  end function
end module

use m
type(dt(4)) x, y
integer i
x%i = 4
y%i = 3
i = x + y
print *, i
end

