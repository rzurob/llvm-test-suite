! Private generic bindings are not accessible outside the module
! they are defined in.

module m
  type base
    integer i
  contains
    private
    procedure :: powerint
    procedure :: powerreal
    procedure :: powerchar
    generic :: operator(**) => powerreal
    generic, private :: operator(**) => powerint
    generic :: operator(**) => powerchar
  end type

  interface operator(**)
    module procedure powerint
  end interface

contains
  type(base) function powerint(x, y)
    class(base), intent(in) :: x
    integer, intent(in) :: y
    powerint%i = x%i + y
  end function

  type(base) function powerreal(x, y)
    class(base), intent(in) :: x
    real, intent(in) :: y
    powerreal%i = x%i - int(y)
  end function

  type(base) function powerchar(x, y)
    class(base), intent(in) :: x
    character, intent(in) :: y
    powerchar%i = x%i * 2
  end function
end module

use m
type(base) :: a = base(3)
type(base) b

b = a ** 1
if (b%i /= 4) then
  print *, b%i
endif

b = a ** 2.0
if (b%i /= 1) then
  print *, b%i
  error stop 1
endif

b = a ** 'c'
if (b%i /= 6) then
  print *, b%i
  error stop 2
endif
end
