!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/15/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Data source missing target attribute.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type container (k)
        integer, kind :: k

        type(base(k,:)), pointer :: data
    end type
end module

program dtparamConstr034d
use m
    type(container(8)) :: co1
    type(base(8, 20)) b1

    co1 = container(8)(b1)
end
