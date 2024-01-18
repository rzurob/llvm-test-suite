! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/unit_tests/diag/genop05cd.f
! opt variations: -ql

! Type-bound generic operators
! Diagnostic: a procedure specified in a specific binding in
!             an abstract type, references a subtype.

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

  function addmp2(x,y)
    class(dt(4)), allocatable :: addmp2
    integer, intent(in) :: x
    class(et(4)), intent(in) :: y
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
    xdt = xdt + 1
    if (xdt%i /= 17) then
      print *, xdt%i
      error stop 1
    endif
  end subroutine
end module

use m
call sub
end
