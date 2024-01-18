! Generic logical operator binding with polymorphism, and deferred bindings.

module m
  type, abstract :: dt
    integer i
  contains
    procedure(addp), pass, deferred :: addthem1
    generic :: OPERATOR(.eq.) => addthem1, addthem2
    procedure, pass(y) :: addthem2 => addmp2
  end type

  interface
    function addp(x,y)
      import dt
      logical :: addp
      class(dt), intent(in) :: x
      integer, intent(in) :: y
    end function addp
  end interface

  type, extends(dt) :: et
  contains
    procedure, pass(x) :: addthem1 => addmp
  end type
contains
  logical function addmp(x,y)
    class(et), intent(in) :: x
    integer, intent(in) :: y
    addmp = (x%i .eq. y + 1)
  end function

  logical function addmp2(x,y)
    integer, intent(in) :: x
    class(dt), intent(in) :: y
    addmp2 = (x + 3 .eq. y%i)
  end function

  subroutine sub
    class(et), allocatable :: xdt
    logical test
    allocate(xdt, source=et(i=5))
    test = (xdt .eq. 4) .and. (2 .eq. xdt)
    if (.not. test) then
      print *, test
      error stop 1_4
    endif
  end subroutine
end module

use m
call sub
end
