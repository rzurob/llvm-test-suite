! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/unit_tests/diag/genop17d.f
! opt variations: -qnol

! specific procedure for an operator must not have an optional arg.

module mod
  type dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)      i
  contains
    procedure :: addthem
    generic :: operator(//) => addthem
  end type
contains
  pure integer function addthem(a, b)
    class(dt(*,4)), intent(in) :: a
    integer, optional,  intent(in) :: b
    addthem = a%i + b + 1
  end function

  subroutine sub
    type(dt(20,4)) :: dta
    dta%i = 3
    i = dta // 4
  end subroutine
end module

use mod
call sub
end
