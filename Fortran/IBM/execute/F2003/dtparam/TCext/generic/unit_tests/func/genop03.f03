! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/unit_tests/func/genop03.f
! opt variations: -qnol

! Generic type-bound with use-only.

module m
  type dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)      i
  contains
    procedure, pass(x) :: add => addmp
    generic :: OPERATOR(+) => add
  end type

  interface OPERATOR(+)
    module procedure addmp
  end interface
contains
  type(dt(20,4)) function addmp(y,x)
    class(dt(*,4)), intent(in) :: x
    integer, intent(in) :: y
    addmp%i = x%i + y + 1
  end function

  subroutine sub
    type(dt(20,4)) xdt
    xdt%i = 5
    xdt = 1 + xdt
    if (xdt%i /= 7) then
      print *, xdt%i
      error stop 1_4
    endif
  end subroutine
end module

use m
type(dt(20,4)) z
call sub
z%i = 7
z = 2 + z
if (z%i /= 10) then
  print *, z%i
  error stop 2_4
endif
call use_interface_only
end

subroutine use_interface_only
  use m, only: dt, OPERATOR(+), addmp
  type(dt(20,4)) z
  z%i = 7
  z = 3 + z
  if (z%i /= 11) then
    print *, z%i
    error stop 3_4
  endif
end subroutine

subroutine use_binding_only
  use m, only: dt, addmp
  type(dt(20,4)) z
  z%i = 7
  z = 3 + z
  if (z%i /= 11) then
    print *, z%i
    error stop 4_4
  endif
end subroutine
