! Simple type-bound defined (+) operator.  integer + derived
! Generic appears before specific.

module m
  type dt
    integer i
  contains
    procedure, pass(x) :: add => addmp
    generic :: OPERATOR(+) => add
  end type
contains
  type(dt) function addmp(y,x)
    class(dt), intent(in) :: x
    integer, intent(in) :: y
    addmp%i = x%i + y + 1
  end function

  subroutine sub
    type(dt) xdt
    xdt%i = 5
    xdt = 1 + xdt
    if (xdt%i /= 7) then
      print *, xdt%i
      error stop 1_4
    endif
  end subroutine
end module

use m
type(dt) z
call sub
z%i = 7
z = 2 + z
if (z%i /= 10) then
  print *, z%i
  error stop 2_4
endif
end
