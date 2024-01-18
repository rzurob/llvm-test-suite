module m
  type dt
  contains
    procedure :: foo
    generic :: gen => foo, bar  ! Error: foo is function, but bar is a sub
    procedure :: bar
  end type
contains
  integer function foo(a)
    class(dt) a
    foo = 3
  end function

  subroutine bar(a)
    class(dt) a
  end subroutine
end module
