! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/unit_tests/diag/genop01d.f
! opt variations: -ql

! Generic bindings:  Error in specific binding referenced in generic binding

module m
  type dt(k1)    ! (4)
    integer, kind :: k1
    integer(k1)      i
  contains
    procedure(addmp), pass :: addmp
    generic :: OPERATOR(+) => addmp
  end type
contains
  type(dt(4)) function addmp(x,y)
    class(dt(4)), intent(in) :: x
    integer, intent(in) :: y
    addmp%i = x%i + y + 1
  end function

  subroutine sub
    type(dt(4)) xdt
    xdt%i = 5
    xdt = xdt + 1
    if (xdt%i /= 7) then
      print *, xdt%i
      error stop 1
    endif
  end subroutine
end module

use m
call sub
end
