! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/unit_tests/diag/genop16bd.f
! opt variations: -ql

! A defined operator must not be the same as any logical literal constant.

module m
  type dt(k1)    ! (4)
    integer, kind :: k1
    integer(k1)      i
  contains
    generic :: operator(.true.) => foo
    procedure :: foo
    generic :: operator(.plus.) => foo
  end type
contains
  integer function foo(a, b)
    class(dt(4)), intent(in) :: a
    integer, intent(in) :: b
    foo = b
  end function
end module
