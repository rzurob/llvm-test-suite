! Simple type-bound defined assignment
! Generic appears before specific.

module m
  type dt
    integer i
  contains
    generic :: ASSIGNMENT(=) => assign
    procedure, pass :: assign => myassign
  end type
contains
  subroutine myassign(x,y)
    class(dt), intent(out) :: x
    integer, intent(in) :: y
    x%i = y + 1
  end subroutine

  subroutine sub
    type(dt) xdt
    xdt = 5
    if (xdt%i /= 6) then
      print *, xdt%i
      error stop 1_4
    endif
  end subroutine
end module

use m
type(dt) z
call sub
z = 1
if (z%i /= 2) then
  print *, z%i
  error stop 2_4
endif
end
