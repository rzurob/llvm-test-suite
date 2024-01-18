! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/unit_tests/diag/genop12d.f
! opt variations: -qnol

module m
  type dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)      i
  contains
    procedure :: foo
    procedure :: bar
    generic :: foo => bar ! Error:  foo already appeared as specific binding
  end type
contains
  integer function foo(a, b)
    class(dt(*,4)), intent(in) :: a
    integer, intent(in) :: b
    foo = b
  end function

  integer function bar(a, b)
    class(dt(*,4)), intent(in) :: a
    real, intent(in) :: b
    bar = int(b)
  end function
end module
