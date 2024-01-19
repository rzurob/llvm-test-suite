! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/F2003/generic/unit_tests/diag/genname02d.f
! opt variations: -qnok -ql -qreuse=none

module m
  type mt(k1)    ! (4)
    integer, kind :: k1
  contains
    procedure, nopass :: foo
    generic, private :: bar => foo
    procedure :: bar  ! error:  bar appeared a generic binding in the same type
  end type

  type dt(k2)    ! (4)
    integer, kind :: k2
    integer(k2)      i
  contains
    procedure, nopass :: foo
    generic, private :: bar => foo
  end type

  type, extends(dt) :: et    ! (4)
    integer(k2) bar  ! error: bar is a binding in the parent type
  end type

  type, extends(dt) :: gt    ! (4)
  contains
    procedure, nopass :: foo
    procedure, nopass :: bar  ! error: bar is a generic binding in the parent type
  end type

  type, extends(dt) :: ht    ! (4)
  contains
    generic :: i => foo  ! error: i is a component in the parent type
  end type

  type, extends(dt) :: qt    ! (4)
  contains
    procedure, nopass :: sub
    generic :: foo => sub  ! error: foo is a specific binding in the parent type
  end type
contains
  subroutine foo(a)
    class(dt(4)) a
  end subroutine

  subroutine bar(a)
    class(dt(4)) a
  end subroutine

  subroutine sub(a)
    integer a
  end subroutine
end module
