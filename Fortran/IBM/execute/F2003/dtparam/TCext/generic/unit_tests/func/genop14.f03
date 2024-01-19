! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/unit_tests/func/genop14.f
! opt variations: -ql

! Unary defined operator binding

module m
  type dt(k1)    ! (4)
    integer, kind :: k1
    integer(k1)      i
  contains
    procedure :: foo
    generic :: operator(.double.) => foo
  end type
contains
  integer function foo(a)
    class(dt(4)), intent(in) :: a
    foo = a%i * 2
  end function

  subroutine sub
    type(dt(4)) :: x = dt(4)(-2)
    integer res
    res = .double. x
    if (res /= -4) then
      print *, res
      error stop 1_4
    endif
  end subroutine
end module

use m
call sub
end
