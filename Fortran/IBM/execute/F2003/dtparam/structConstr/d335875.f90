!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/20/2007
!*
!*  DESCRIPTION                : miscellaneous
!                               defect 335875
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k = 4

        procedure(integer(k)), nopass, pointer :: proc
    end type
end module

use m
    type(base(8)) b1
    type(base) b2

    integer(8), external :: calc1
    integer(4), external :: calc2

    b1%proc => calc1
    b2%proc => calc2

    if (b1%proc(base(null()), -10) /= -10) error stop 1_4

    if (b1%proc(b2, 11) /= 9) error stop 2_4
end

integer(8) function calc1 (b1, i1)
use m
    type(base), intent(in) :: b1

    if (associated(b1%proc)) then
        calc1 = b1%proc(i1) ** 2
    else
        calc1 = i1
    end if
end function

integer(4) function calc2 (i1)
    integer, intent(in) :: i1

    calc2 = mod(i1,8)
end function
