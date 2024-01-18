! Type-bound generic assignment
! Two generic assignment bindings point to different specific bindings.
! The specific bindings point to the same procedure.

module m
  type dt
    integer i
  contains
    generic :: ASSIGNMENT(=) => assign
    procedure, pass :: assign => myassign
    procedure, pass :: otherassign => myassign
    generic :: ASSIGNMENT(=) => otherassign
  end type
contains
  subroutine myassign(x,y)
    class(dt), intent(out) :: x
    integer, intent(in) :: y
    x%i = y + 1
  end subroutine

  subroutine myotherassign(x,y)
    class(dt), intent(out) :: x
    integer, intent(in) :: y
    x%i = y + 3
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
