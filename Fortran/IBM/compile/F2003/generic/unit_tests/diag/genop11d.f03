module m
  type dt
    integer i
  contains
    procedure, nopass :: foo
    generic :: operator(.add.) => foo ! Error: foo must be pass
  end type
contains
  integer function foo(a, b)
    class(dt), intent(in) :: a
    integer, intent(in) :: b
    foo = b
  end function
end module
