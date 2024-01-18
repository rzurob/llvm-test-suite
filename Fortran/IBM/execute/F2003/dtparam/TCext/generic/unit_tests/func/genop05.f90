! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/unit_tests/func/genop05.f
! opt variations: -qnol -qnodeferredlp

! Generic operator bindings: polymorphism.

module m
  type, abstract :: dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)      i
  contains
    procedure(addp), pass, deferred :: addthem
    generic :: OPERATOR(+) => addthem
  end type

  interface
    function addp(x,y)
      import dt
      class(dt(:,4)), allocatable :: addp
      class(dt(*,4)), intent(in) :: x
      integer, intent(in) :: y
    end function addp
  end interface

  type, extends(dt) :: et    ! (20,4)
  contains
    procedure, pass(x) :: addthem => addmp
    procedure assignmp
    generic :: ASSIGNMENT(=) => assignmp
  end type
contains
  function addmp(x,y)
    class(dt(:,4)), allocatable :: addmp
    class(et(*,4)), intent(in) :: x
    integer, intent(in) :: y
    allocate(addmp, source=x)
    addmp%i = x%i + y + 1
  end function

  subroutine assignmp(x,y)
    class(et(*,4)), intent(out) :: x
    class(dt(*,4)), intent(in) :: y
    x%i = y%i + 10
  end subroutine

  subroutine sub
    class(et(:,4)), allocatable :: xdt
    allocate(xdt, source=et(20,4)(i=5))
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
