! type-bound generic names, polymorphism

module m
  type, abstract :: dt
    integer i
  contains
    procedure(addp), pass, deferred :: addthem1
    generic :: foo => addthem1, addthem2
    procedure, pass(y2) :: addthem2 => addmp2
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
    generic :: foo => addthem3
    procedure, pass(y3) :: addthem3
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

  function addmp2(x2,y2,c2)
    class(dt), allocatable :: addmp2
    integer, intent(in) :: x2
    class(dt), intent(in) :: y2
    integer, intent(in) :: c2
    allocate(addmp2, source=y2)
    addmp2%i = x2 + y2%i + 3 + c2
  end function

  function addthem3(x3,y3)
    class(et), allocatable :: addthem3
    real, intent(in) :: x3
    class(et), intent(in) :: y3
    allocate(addthem3, source=y3)
    addthem3%i = int(x3) + y3%i + 20
  end function

  subroutine assignmp(xa,ya)
    class(et), intent(out) :: xa
    class(dt), intent(in) :: ya
    xa%i = ya%i + 10
  end subroutine

  subroutine sub
    class(et), allocatable :: xdt
    allocate(xdt, source=et(i=5))
    xdt = xdt%foo(1)
    if (xdt%i /= 17) then
      print *, xdt%i
      error stop 1_4
    endif
    xdt = xdt%foo(1,2)
    if (xdt%i /= 33) then
      print *, xdt%i
      error stop 2_4
    endif
    xdt = xdt%foo(2.0)
    if (xdt%i /= 65) then
      print *, xdt%i
      error stop 3_4
    endif
  end subroutine
end module

use m
!type(et), target :: z = et(1)
!class(et), pointer :: zp
call sub
!z = 1 + z + 2
!if (z%i /= 18) then
!  print *, z%i
!  error stop 2_4
!endif
!zp => z
!zp = 1 + zp + 5
!if (z%i /= 38) then
!  print *, z%i
!  error stop 3_4
!endif
end
