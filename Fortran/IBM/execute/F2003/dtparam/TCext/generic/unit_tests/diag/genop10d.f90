! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/unit_tests/diag/genop10d.f
! opt variations: -qnol

! All generic bindings with the same generic specification in the same
! type must have the same accessibility.
! This case:  gracefully handle an error in the specific binding

module m
  type base(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    real(k1)         r
  contains
    procedure :: powerint, powerreal, powerchar
    generic, private :: operator(**) => powerint
    generic, public :: operator(**) => powerreal
    generic :: operator(**) => powerchar
  end type

contains
  type(base(20,4)) function powerint(x, y)
    class(base(*,4)), intent(in) :: x
    integer, intent(in) :: y
    powerint%r = x%r + real(y)
  end function

  type(base(20,4)) function powerreal(x, y)
    class(base(*,4)), intent(in) :: x
    real, intent(in) :: y
    powerreal%r = x%r - y
  end function

  type(base(20,4)) function powerchar(x, y)
    class(base(*,4)), intent(in) :: x
    character, intent(in) :: y
    powerchar%r = x%r * 2.0
  end function
end module
