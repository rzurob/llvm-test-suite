! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/unit_tests/func/genop10.f
! opt variations: -ql

! Accessiblity of generic bindings.

module m
  type base(k1)    ! (4)
    integer, kind :: k1
    integer(k1)      i
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
  type(base(4)) function powerint(x, y)
    class(base(4)), intent(in) :: x
    integer, intent(in) :: y
    powerint%i = x%i + y
  end function

  type(base(4)) function powerreal(x, y)
    class(base(4)), intent(in) :: x
    real, intent(in) :: y
    powerreal%i = x%i - int(y)
  end function

  type(base(4)) function powerchar(x, y)
    class(base(4)), intent(in) :: x
    character, intent(in) :: y
    powerchar%i = x%i * 2
  end function
end module

use m
type(base(4)) :: a = base(4)(3)
type(base(4)) b

b = a ** 1
if (b%i /= 4) then
  print *, b%i
  error stop 1_4
endif
end
