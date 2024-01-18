! Unary defined operator binding

module m
  type dt
    integer i
  contains
    procedure :: foo
    generic :: operator(.double.) => foo
  end type
contains
  integer function foo(a)
    class(dt), intent(in) :: a
    foo = a%i * 2
  end function

  subroutine sub
    type(dt) :: x = dt(-2)
    integer res
    res = .double. x
    if (res /= -4) then
      print *, res
      error stop 1_4
    endif
  end subroutine
end module

use m
call sub
end
