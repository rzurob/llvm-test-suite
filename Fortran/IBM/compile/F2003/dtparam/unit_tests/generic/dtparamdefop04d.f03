! Defined operator diagnostic: length type parameter does not match
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
    class(dt(4,*)), intent(in) :: a
    class(dt(4,4)), intent(in) :: b
    integer j
    print *, a%l
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
print *, i
end

