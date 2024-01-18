! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/unit_tests/diag/genop11d.f
! opt variations: -ql

module m
  type dt(k1)    ! (4)
    integer, kind :: k1
    integer(k1)      i
  contains
    procedure, nopass :: foo
    generic :: operator(.add.) => foo ! Error: foo must be pass
  end type
contains
  integer function foo(a, b)
    class(dt(4)), intent(in) :: a
    integer, intent(in) :: b
    foo = b
  end function
end module
