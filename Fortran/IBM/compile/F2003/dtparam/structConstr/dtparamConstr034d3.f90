!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/16/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use procedure target whose interface
!                               mismatches with pointer component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        procedure(real(k)), nopass, pointer :: p
    end type

    contains

    real(4) function getReal (i, p)
        integer, intent(in) :: i(*)
        procedure(real(4)) :: p

        getReal = p(sum(i(:10)))
    end function
end module

program dtparamConstr034d3
use m
    type(base(8, 10)) b1

    b1 = base(8, 10)((/(i, i=1,10)/), getReal)
end
