! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/unit_tests/func/genop05b.f
! opt variations: -ql

! Generic operator bindings: polymorphism, expressions.

module m
  type, abstract :: dt(k1)    ! (4)
    integer, kind :: k1
    integer(k1)      i
  contains
    procedure(addp), pass, deferred :: addthem1
    generic :: OPERATOR(+) => addthem1, addthem2
    procedure, pass(y) :: addthem2 => addmp2
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
    procedure, pass(x) :: addthem1 => addmp
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

  function addmp2(x,y)
    class(dt(4)), allocatable :: addmp2
    integer, intent(in) :: x
    class(dt(4)), intent(in) :: y
    allocate(addmp2, source=y)
    addmp2%i = x + y%i + 3
  end function

  subroutine assignmp(x,y)
    class(et(4)), intent(out) :: x
    class(dt(4)), intent(in) :: y
    x%i = y%i + 10
  end subroutine

  subroutine sub
    class(et(4)), allocatable :: xdt
    allocate(xdt, source=et(4)(i=5))
    xdt = (1 + xdt) + 1
    if (xdt%i /= 21) then
      print *, xdt%i
      error stop 1_4
    endif
  end subroutine
end module

use m
type(et(4)), target :: z = et(4)(1)
class(et(4)), pointer :: zp
call sub
z = 1 + z + 2
if (z%i /= 18) then
  print *, z%i
  error stop 2_4
endif
zp => z
zp = 1 + zp + 5
if (z%i /= 38) then
  print *, z%i
  error stop 3_4
endif
end
