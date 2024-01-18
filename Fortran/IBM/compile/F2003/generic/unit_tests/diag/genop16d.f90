! A defined operator must not be the same as any logical literal constant.

module m
  type dt
    integer i
  contains
    procedure :: foo
    generic :: operator(.true.) => foo
    generic :: operator(.false.) => foo, foo
  end type
contains
  subroutine foo(a)
    class(dt), intent(in) :: a
  end subroutine
end module
