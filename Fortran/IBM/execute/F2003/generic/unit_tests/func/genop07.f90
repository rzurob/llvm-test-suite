! Elemental generic operator binding with polymorphism and deferred bindings.

module m
  type, abstract :: dt
    integer i
  contains
    procedure(addp), pass, deferred :: addthem1
    generic :: OPERATOR(+) => addthem1, addthem2
    procedure, pass(y) :: addthem2 => addmp2
  end type

  type, extends(dt) :: et
  contains
    procedure, pass(x) :: addthem1 => addmp
    procedure assignmp
    generic :: ASSIGNMENT(=) => assignmp
  end type

  interface
    elemental function addp(x,y)
      import dt, et
      type(et) :: addp
      class(dt), intent(in) :: x
      integer, intent(in) :: y
    end function addp
  end interface
contains
  elemental function addmp(x,y)
    type(et) :: addmp
    class(et), intent(in) :: x
    integer, intent(in) :: y
    addmp%i = x%i + y + 1
  end function

  elemental function addmp2(x,y)
    type(et) :: addmp2
    integer, intent(in) :: x
    class(dt), intent(in) :: y
    addmp2%i = x + y%i + 3
  end function

  elemental subroutine assignmp(x,y)
    class(et), intent(out) :: x
    class(dt), intent(in) :: y
    x%i = y%i + 10
  end subroutine

  subroutine sub
    class(et), allocatable :: xdt(:)
    integer, parameter :: vf(3) = (/ 17, 18, 19 /)
    integer i
    allocate(xdt(3), source=(/ (et(i=i), i=1, 3) /) )
    xdt = (1 + xdt) + 1
    do i = 1, 3
      if (xdt(i)%i /= vf(i)) then
        print *, i, xdt(i)%i
        call zzrc(i)
      endif
    enddo
  end subroutine
end module

use m
call sub
end
