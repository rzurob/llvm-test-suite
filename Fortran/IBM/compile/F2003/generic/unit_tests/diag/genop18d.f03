! Generic interface block conflicts with generic binding.
! (With the generic binding appearing first)

module m
  type dt
  contains
    procedure :: foo
    generic :: operator(+) => foo
  end type
contains
  integer function foo(a,i)
    class(dt), intent(in) :: a
    integer, intent(in) :: i
    print *, "foo"
    foo = i
  end function
end module

use m
interface operator(+)
  integer function bar(a,i)
    use m, only: dt
    class(dt), intent(in) :: a
    integer, intent(in) :: i
  end function
end interface
type(dt) a
integer ii
ii = a + 4
end

integer function bar(a,i)
  use m, only: dt
  class(dt), intent(in) :: a
  integer, intent(in) :: i
  print *, "bar"
  bar = i+1
end function
