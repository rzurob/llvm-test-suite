! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/unit_tests/func/genop04.f
! opt variations: -ql

! Generic operator bindings: inheritance

module m
  type, abstract :: dt(k1)    ! (4)
    integer, kind :: k1
    integer(k1)      i
  contains
    procedure(addp), pass, deferred :: addthem
    generic :: OPERATOR(+) => addthem
  end type

  interface
    function addp(x,y)
      import dt
      class(dt(4)), allocatable :: addp
      class(dt(4)), intent(in) :: x
      integer, intent(in) :: y
    end function addp
  end interface

  type, extends(dt) :: et    ! (4)
  contains
    procedure, pass(x) :: addthem => addmp
    procedure assignmp
    generic :: ASSIGNMENT(=) => assignmp
  end type
contains
  function addmp(x,y)
    class(dt(4)), allocatable :: addmp
    class(et(4)), intent(in) :: x
    integer, intent(in) :: y
    allocate(addmp, source=x)
    addmp%i = x%i + y + 1
  end function

  subroutine assignmp(x,y)
    class(et(4)), intent(out) :: x
    class(dt(4)), intent(in) :: y
    x%i = y%i + 10
  end subroutine

  subroutine sub
    type(et(4)) xdt
    xdt%i = 5
    xdt = xdt + 1
    if (xdt%i /= 17) then
      print *, xdt%i
      error stop 1_4
    endif
  end subroutine
end module

use m
type(et(4)) z
call sub
z%i = 1
z = z + 1
if (z%i /= 13) then
  print *, z%i
  error stop 2_4
endif
end
