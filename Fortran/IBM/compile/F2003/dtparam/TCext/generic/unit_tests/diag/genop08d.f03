! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/unit_tests/diag/genop08d.f
! opt variations: -qnol

! Type-bound generic operators.
! Diagnostic: dt1 + dt2 where each dt has a generic operator binding
!             for dt1 + dt2.

module m
  type dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)      i
  contains
    procedure, pass(x) :: add => addmp
    generic :: OPERATOR(+) => add
  end type

  type td(n2,k2)    ! (20,4)
    integer, kind :: k2
    integer, len  :: n2
    real(k2)         j
  contains
    procedure, pass(y) :: add => addmp2
    generic :: OPERATOR(+) => add
  end type
contains
  type(dt(20,4)) function addmp(x,y)
    class(dt(*,4)), intent(in) :: x
    class(td(*,4)), intent(in) :: y
    addmp%i = x%i + y%j + 1
  end function

  type(td(20,4)) function addmp2(x,y)
    class(dt(*,4)), intent(in) :: x
    class(td(*,4)), intent(in) :: y
    addmp2%j = x%i + y%j + 4
  end function

  subroutine sub
    type(dt(20,4)) :: xdt = dt(20,4)(5)
    type(td(20,4)) :: xtd = td(20,4)(4.0)
    xdt = xdt + xtd
    if (xdt%i /= 10) then
      print *, xdt%i
      error stop 1
    endif
    xtd = xtd + xdt
    if (xtd%j /= 18) then
      print *, xtd%j
      error stop 2
    endif
  end subroutine
end module

use m
call sub
end
