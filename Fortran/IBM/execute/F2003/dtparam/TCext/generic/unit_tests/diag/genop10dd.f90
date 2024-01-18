! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/unit_tests/diag/genop10dd.f
! opt variations: -qnol

! Private generic bindings are not accessible outside the module
! they are defined in.

module m
  type base(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
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
  type(base(20,4)) function powerint(x, y)
    class(base(*,4)), intent(in) :: x
    integer, intent(in) :: y
    powerint%i = x%i + y
  end function

  type(base(20,4)) function powerreal(x, y)
    class(base(*,4)), intent(in) :: x
    real, intent(in) :: y
    powerreal%i = x%i - int(y)
  end function

  type(base(20,4)) function powerchar(x, y)
    class(base(*,4)), intent(in) :: x
    character, intent(in) :: y
    powerchar%i = x%i * 2
  end function
end module

use m
type(base(20,4)) :: a = base(20,4)(3)
type(base(20,4)) b

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
