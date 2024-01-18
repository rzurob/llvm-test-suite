! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/unit_tests/func/genop13.f
! opt variations: -qnol

! Binary defined operator binding

module m
  type dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)      i
  contains
    procedure :: foo
    generic :: operator(.add.) => foo
  end type
contains
  integer function foo(a, b)
    class(dt(*,4)), intent(in) :: a
    integer, intent(in) :: b
    foo = a%i * b
  end function

  subroutine sub
    type(dt(20,4)) :: x = dt(20,4)(-2)
    integer res
    res = x .add. 3
    if (res /= -6) then
      print *, res
      error stop 1_4
    endif
  end subroutine
end module

use m
type(dt(20,4)) :: z
integer i
call sub
z%i = 5
i = z .add. -2
if (i /= -10) then
  print *, i
  error stop 2_4
endif
end
