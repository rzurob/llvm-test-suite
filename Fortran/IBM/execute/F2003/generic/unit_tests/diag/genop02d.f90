! Type-bound generic operators
! Diagnostic: If we have a binding for (integer + dt), the
!             binding won't apply to (dt + integer).

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
    xdt = xdt + 1
    if (xdt%i /= 7) then
      print *, xdt%i
      error stop 1
    endif
  end subroutine
end module

use m
type(dt) z
call sub
z%i = 7
z = z + 2
if (z%i /= 10) then
  print *, z%i
  error stop 2
endif
end
