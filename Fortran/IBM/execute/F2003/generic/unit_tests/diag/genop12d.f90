module m
  type dt
    integer i
  contains
    procedure :: foo
    procedure :: bar
    generic :: foo => bar ! Error:  foo already appeared as specific binding
  end type
contains
  integer function foo(a, b)
    class(dt), intent(in) :: a
    integer, intent(in) :: b
    foo = b
  end function

  integer function bar(a, b)
    class(dt), intent(in) :: a
    real, intent(in) :: b
    bar = int(b)
  end function
end module
