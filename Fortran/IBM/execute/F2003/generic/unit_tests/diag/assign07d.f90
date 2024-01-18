! Diagnostic Test Case
! Type-bound generic assignment
!
! - Two generic bindings for the same types point to specific
!   bindings that are bound to different procedures.
! - Assignment between incompatible types that don't have
!   defined assignment
module m
  type dt
    integer i
  contains
    generic :: ASSIGNMENT(=) => assign
    procedure, pass :: assign => myassign
    procedure, pass :: otherassign => myotherassign
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
    print *, xdt%i
    xdt = 6.3
    print *, xdt%i
  end subroutine
end module

use m
type(dt) z
call sub
end
