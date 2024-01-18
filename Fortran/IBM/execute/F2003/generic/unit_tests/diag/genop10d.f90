! All generic bindings with the same generic specification in the same
! type must have the same accessibility.
! This case:  gracefully handle an error in the specific binding

module m
  type base
    real r
  contains
    procedure :: powerint, powerreal, powerchar
    generic, private :: operator(**) => powerint
    generic, public :: operator(**) => powerreal
    generic :: operator(**) => powerchar
  end type

contains
  type(base) function powerint(x, y)
    class(base), intent(in) :: x
    integer, intent(in) :: y
    powerint%r = x%r + real(y)
  end function

  type(base) function powerreal(x, y)
    class(base), intent(in) :: x
    real, intent(in) :: y
    powerreal%r = x%r - y
  end function

  type(base) function powerchar(x, y)
    class(base), intent(in) :: x
    character, intent(in) :: y
    powerchar%r = x%r * 2.0
  end function
end module
