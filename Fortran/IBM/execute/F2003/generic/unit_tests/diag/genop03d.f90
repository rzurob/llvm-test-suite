! Type-bound generic operators
! Diagnostic: A generic binding and a generic interface
!             have different interface bodies for the same
!             operators (and arg types).

module m
  type dt
    integer i
  contains
    procedure, pass(x) :: add => addmp
    generic :: OPERATOR(+) => add
  end type

  interface OPERATOR(+)
    module procedure addmp2
  end interface
contains
  type(dt) function addmp(y,x)
    integer, intent(in) :: y
    class(dt), intent(in) :: x
    addmp%i = x%i + y + 1
  end function

  type(dt) function addmp2(y,x)
    integer, intent(in) :: y
    class(dt), intent(in) :: x
    addmp2%i = x%i + y + 10
  end function

  subroutine sub
    type(dt) xdt
    xdt%i = 5
    xdt = 1 + xdt
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
z = 2 + z
if (z%i /= 10) then
  print *, z%i
  error stop 2
endif
call use_interface_only
end

subroutine use_interface_only
  use m, only: dt, OPERATOR(+), addmp2
  type(dt) z
  z%i = 7
  z = 3 + z
  if (z%i /= 20) then
    print *, z%i
    error stop 3
  endif
end subroutine

subroutine use_binding_only
  use m, only: dt, addmp
  type(dt) z
  z%i = 7
  z = 3 + z
  if (z%i /= 11) then
    print *, z%i
    error stop 4
  endif
end subroutine

