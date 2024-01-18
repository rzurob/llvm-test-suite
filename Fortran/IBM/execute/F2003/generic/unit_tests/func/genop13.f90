! Binary defined operator binding

module m
  type dt
    integer i
  contains
    procedure :: foo
    generic :: operator(.add.) => foo
  end type
contains
  integer function foo(a, b)
    class(dt), intent(in) :: a
    integer, intent(in) :: b
    foo = a%i * b
  end function

  subroutine sub
    type(dt) :: x = dt(-2)
    integer res
    res = x .add. 3
    if (res /= -6) then
      print *, res
      error stop 1_4
    endif
  end subroutine
end module

use m
type(dt) :: z
integer i
call sub
z%i = 5
i = z .add. -2
if (i /= -10) then
  print *, i
  error stop 2_4
endif
end
