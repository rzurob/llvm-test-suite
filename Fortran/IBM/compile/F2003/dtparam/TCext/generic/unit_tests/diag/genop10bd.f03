! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/unit_tests/diag/genop10bd.f
! opt variations: -ql

! All generic bindings with the same generic specification in the same
! type must have the same accessibility.
! This case:  private, public, default public

module m
  type base(k1)    ! (4)
    integer, kind :: k1
    integer(k1)      i
  contains
    procedure :: powerint
    procedure :: powerreal
    procedure :: powerchar
    generic, private :: operator(**) => powerint
    generic, public :: operator(**) => powerreal
    generic :: operator(**) => powerchar
  end type

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
