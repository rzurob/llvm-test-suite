! type-bound generic assignment
!   Define assignment for dt=integer and dt=real
!   All generic bindings appear before specific ones.

module m
  type dt
    integer i
  contains
    generic :: ASSIGNMENT(=) => assign, assignReal
    procedure, pass :: assign => myassign
    procedure, pass :: assignReal => myassignReal
  end type
contains
  subroutine myassign(x,y)
    class(dt), intent(out) :: x
    integer, intent(in) :: y
    x%i = y + 1
  end subroutine

  subroutine myassignReal(x,y)
    class(dt), intent(out) :: x
    real, intent(in) :: y
    x%i = ceiling(y) + 10
  end subroutine

  subroutine sub
    type(dt) xdt
    xdt = 5
    if (xdt%i /= 6) then
      print *, xdt%i
      error stop 1_4
    endif

    xdt = 6.3
    if (xdt%i /= 17) then
      print *, xdt%i
      error stop 2_4
    endif
  end subroutine
end module

use m
type(dt) z

call sub

z = 1
if (z%i /= 2) then
  print *, z%i
  error stop 3_4
endif

z = 9.5
if (z%i /= 20) then
  print *, z%i
  error stop 4_4
endif
end
