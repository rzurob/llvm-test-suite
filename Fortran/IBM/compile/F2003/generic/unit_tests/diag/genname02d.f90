module m
  type mt
  contains
    procedure, nopass :: foo
    generic, private :: bar => foo
    procedure :: bar  ! error:  bar appeared a generic binding in the same type
  end type

  type dt
    integer i
  contains
    procedure, nopass :: foo
    generic, private :: bar => foo
  end type

  type, extends(dt) :: et
    integer bar  ! error: bar is a binding in the parent type
  end type

  type, extends(dt) :: gt
  contains
    procedure, nopass :: foo
    procedure, nopass :: bar  ! error: bar is a generic binding in the parent type
  end type

  type, extends(dt) :: ht
  contains
    generic :: i => foo  ! error: i is a component in the parent type
  end type

  type, extends(dt) :: qt
  contains
    procedure, nopass :: sub
    generic :: foo => sub  ! error: foo is a specific binding in the parent type
  end type
contains
  subroutine foo(a)
    class(dt) a
  end subroutine

  subroutine bar(a)
    class(dt) a
  end subroutine

  subroutine sub(a)
    integer a
  end subroutine
end module
