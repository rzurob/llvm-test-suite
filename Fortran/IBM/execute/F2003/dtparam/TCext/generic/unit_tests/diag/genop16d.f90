! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/unit_tests/diag/genop16d.f
! opt variations: -qnol

! A defined operator must not be the same as any logical literal constant.

module m
  type dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)      i
  contains
    procedure :: foo
    generic :: operator(.true.) => foo
    generic :: operator(.false.) => foo, foo
  end type
contains
  subroutine foo(a)
    class(dt(*,4)), intent(in) :: a
  end subroutine
end module
