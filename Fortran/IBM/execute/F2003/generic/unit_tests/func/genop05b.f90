! Generic operator bindings: polymorphism, expressions.

module m
  type, abstract :: dt
    integer i
  contains
    procedure(addp), pass, deferred :: addthem1
    generic :: OPERATOR(+) => addthem1, addthem2
    procedure, pass(y) :: addthem2 => addmp2
  end type

  interface
    function addp(x,y)
      import dt
      class(dt), allocatable :: addp
      class(dt), intent(in) :: x
      integer, intent(in) :: y
    end function addp
  end interface

  type, extends(dt) :: et
  contains
    procedure, pass(x) :: addthem1 => addmp
    procedure assignmp
    generic :: ASSIGNMENT(=) => assignmp
  end type
contains
  function addmp(x,y)
    class(dt), allocatable :: addmp
    class(et), intent(in) :: x
    integer, intent(in) :: y
    allocate(addmp, source=x)
    addmp%i = x%i + y + 1
  end function

  function addmp2(x,y)
    class(dt), allocatable :: addmp2
    integer, intent(in) :: x
    class(dt), intent(in) :: y
    allocate(addmp2, source=y)
    addmp2%i = x + y%i + 3
  end function

  subroutine assignmp(x,y)
    class(et), intent(out) :: x
    class(dt), intent(in) :: y
    x%i = y%i + 10
  end subroutine

  subroutine sub
    class(et), allocatable :: xdt
    allocate(xdt, source=et(i=5))
    xdt = (1 + xdt) + 1
    if (xdt%i /= 21) then
      print *, xdt%i
      error stop 1_4
    endif
  end subroutine
end module

use m
type(et), target :: z = et(1)
class(et), pointer :: zp
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
