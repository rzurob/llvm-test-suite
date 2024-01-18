! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/unit_tests/func/assign03b.f
! opt variations: -ql

! Simple type-bound defined assignment
! Specific appears before generic.

module m
  type dt(k1)    ! (4)
    integer, kind :: k1
    integer(k1)      i
  contains
    procedure, pass :: assign => myassign
    generic :: ASSIGNMENT(=) => assign
  end type
contains
  subroutine myassign(x,y)
    class(dt(4)), intent(out) :: x
    integer, intent(in) :: y
    x%i = y + 1
  end subroutine

  subroutine sub
    type(dt(4)) xdt
    xdt = 5
    if (xdt%i /= 6) then
      print *, xdt%i
      error stop 1_4
    endif
  end subroutine
end module

use m
type(dt(4)) z
call sub
z = 1
if (z%i /= 2) then
  print *, z%i
  error stop 2_4
endif
end
