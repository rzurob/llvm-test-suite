! A defined operator must not be the same as any logical literal constant.

module m
  type dt
    integer i
  contains
    generic :: operator(.true.) => foo
    procedure :: foo
    generic :: operator(.plus.) => foo
  end type
contains
  integer function foo(a, b)
    class(dt), intent(in) :: a
    integer, intent(in) :: b
    foo = b
  end function
end module
