! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/unit_tests/func/assign04.f
! opt variations: -qnol

! type-bound generic assignment
!   Define assignment for dt=integer and dt=real
!   Some generic appears before specific

module m
  type dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)      i
  contains
    generic :: ASSIGNMENT(=) => assign!, assignReal
    procedure, pass :: assign => myassign
    procedure, pass :: assignReal => myassignReal
    generic :: ASSIGNMENT(=) => assignReal
  end type
contains
  subroutine myassign(x,y)
    class(dt(*,4)), intent(out) :: x
    integer, intent(in) :: y
    x%i = y + 1
  end subroutine

  subroutine myassignReal(x,y)
    class(dt(*,4)), intent(out) :: x
    real, intent(in) :: y
    x%i = ceiling(y) + 10
  end subroutine

  subroutine sub
    type(dt(20,4)) xdt
    xdt = 5
    if (xdt%i /= 6) then
      print *, xdt%i
      error stop 1_4
    endif

    xdt = 6.3
    if (xdt%i /= 17) then
      print *, xdt%i
      error stop 2_4
    endif
  end subroutine
end module

use m
type(dt(20,4)) z

call sub

z = 1
if (z%i /= 2) then
  print *, z%i
  error stop 3_4
endif

z = 9.5
if (z%i /= 20) then
  print *, z%i
  error stop 4_4
endif
end
