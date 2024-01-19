! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/unit_tests/func/genop15.f
! opt variations: -qnol

! Generic operator bindings with FORALL and WHERE

module mod
  type dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)      i
  contains
    procedure :: addthem
    generic :: operator(.add.) => addthem
    generic :: operator(/=) => nequal
    procedure :: nequal => neqint
    generic :: assignment(=) => assign
    procedure :: assign
  end type
contains
  pure type(dt(20,4)) function addthem(a, b)
    class(dt(*,4)), intent(in) :: a
    integer, intent(in) :: b
    addthem = a%i + b + 1
  end function

  pure type(dt(20,4)) function getdt(a)
    class(dt(*,4)), intent(in) :: a
    getdt = a
  end function

  elemental logical function neqint(a, b)
    class(dt(*,4)), intent(in) :: a
    integer, intent(in) :: b
    neqint = (abs(a%i) /= abs(b))
  end function

  elemental subroutine assign(a, b)
    class(dt(*,4)), intent(out) :: a
    integer, intent(in) :: b
    a%i = -b
  end subroutine

  subroutine sub
    integer :: m
    type(dt(20,4)) :: a(10), dtarray(10) = (/ (dt(20,4)(m), m=1, 10) /)
    integer :: x, i=1, j=10

    ! Test forall
    forall (x=i:j)
      a(x) = getdt(dtarray(x)) .add. x
    end forall
    ! verify
    if (any(a /= (/ (-m, m=3, 21, 2) /))) then
      do m=1,10
        print *, a(m)%i
      enddo
      error stop 1_4
    endif

    ! Test where
    where (a .ne. 11) a = 22
    ! verify
    if (any(a(1:4) /= -22)) then
      error stop 2_4
    end if
    if (a(5) /= 11) then
      print *, a(5)%i
      error stop 3_4
    endif
    if (any(a(6:) /= -22)) then
      error stop 4_4
    end if
  end subroutine
end module

use mod
call sub
end
