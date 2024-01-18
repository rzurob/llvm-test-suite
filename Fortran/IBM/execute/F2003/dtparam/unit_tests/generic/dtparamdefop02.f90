! Simple defined + operator with kind and assumed length type parameters
module m
  type dt(k,l)
    integer, kind :: k
    integer, len :: l
    integer(k) i(l)
  contains
    procedure :: add => f1
    generic :: operator(+) => add
  end type
contains
  integer function f1(a,b)
    class(dt(4,*)), intent(in) :: a,b
    integer j
    f1 = 0
    do j = 1, a%l
      f1 = f1 + a%i(j) + b%i(j)
    end do
  end function
end module

use m
type(dt(4,2)) x, y
integer i
x%i = 4
y%i = 3
i = x + y
if (i /= 14) then
  print *, i
  error stop 1
endif
end

