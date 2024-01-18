! Generic operator bindings: polymorphism.

module m
  type, abstract :: dt
    integer i
  contains
    procedure(addp), pass, deferred :: addthem
    generic :: OPERATOR(+) => addthem
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
    procedure, pass(x) :: addthem => addmp
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

  subroutine assignmp(x,y)
    class(et), intent(out) :: x
    class(dt), intent(in) :: y
    x%i = y%i + 10
  end subroutine

  subroutine sub
    class(et), allocatable :: xdt
    allocate(xdt, source=et(i=5))
    xdt = xdt + 1
    if (xdt%i /= 17) then
      print *, xdt%i
      error stop 1_4
    endif
  end subroutine
end module

use m
call sub
end
