! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/generic/unit_tests/func/genop07.f
! opt variations: -qnok -qnol -qnodeferredlp

! Elemental generic operator binding with polymorphism and deferred bindings.

module m
  type, abstract :: dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)      i
  contains
    procedure(addp), pass, deferred :: addthem1
    generic :: OPERATOR(+) => addthem1, addthem2
    procedure, pass(y) :: addthem2 => addmp2
  end type

  type, extends(dt) :: et(k2,n2)    ! (20,4,4,20)
      integer, kind :: k2
      integer, len  :: n2
  contains
    procedure, pass(x) :: addthem1 => addmp
    procedure assignmp
    generic :: ASSIGNMENT(=) => assignmp
  end type

  interface
    elemental function addp(x,y)
      import dt, et
      type(et(20,4,4,20)) :: addp
      class(dt(*,4)), intent(in) :: x
      integer, intent(in) :: y
    end function addp
  end interface
contains
  elemental function addmp(x,y)
    type(et(20,4,4,20)) :: addmp
    class(et(*,4,4,*)), intent(in) :: x
    integer, intent(in) :: y
    addmp%i = x%i + y + 1
  end function

  elemental function addmp2(x,y)
    type(et(20,4,4,20)) :: addmp2
    integer, intent(in) :: x
    class(dt(*,4)), intent(in) :: y
    addmp2%i = x + y%i + 3
  end function

  elemental subroutine assignmp(x,y)
    class(et(*,4,4,*)), intent(out) :: x
    class(dt(*,4)), intent(in) :: y
    x%i = y%i + 10
  end subroutine

  subroutine sub
    class(et(:,4,4,:)), allocatable :: xdt(:)
    integer, parameter :: vf(3) = (/ 17, 18, 19 /)
    integer i
    allocate(xdt(3), source=(/ (et(20,4,4,20)(i=i), i=1, 3) /) )
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
