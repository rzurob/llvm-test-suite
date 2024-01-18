! Generic bindings:  Error in specific binding referenced in generic binding

module m
  type dt
    integer i
  contains
    procedure(addmp), pass :: addmp
    generic :: OPERATOR(+) => addmp
  end type
contains
  type(dt) function addmp(x,y)
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
call sub
end
