! Type-bound generic operators.
! Diagnostic: dt1 + dt2 where each dt has a generic operator binding
!             for dt1 + dt2.

module m
  type dt
    integer i
  contains
    procedure, pass(x) :: add => addmp
    generic :: OPERATOR(+) => add
  end type

  type td
    real j
  contains
    procedure, pass(y) :: add => addmp2
    generic :: OPERATOR(+) => add
  end type
contains
  type(dt) function addmp(x,y)
    class(dt), intent(in) :: x
    class(td), intent(in) :: y
    addmp%i = x%i + y%j + 1
  end function

  type(td) function addmp2(x,y)
    class(dt), intent(in) :: x
    class(td), intent(in) :: y
    addmp2%j = x%i + y%j + 4
  end function

  subroutine sub
    type(dt) :: xdt = dt(5)
    type(td) :: xtd = td(4.0)
    xdt = xdt + xtd
    if (xdt%i /= 10) then
      print *, xdt%i
      error stop 1
    endif
    xtd = xtd + xdt
    if (xtd%j /= 18) then
      print *, xtd%j
      error stop 2
    endif
  end subroutine
end module

use m
call sub
end
