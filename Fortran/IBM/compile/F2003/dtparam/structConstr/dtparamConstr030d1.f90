!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/09/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use derived types with different kind
!                               parameters: the component is parameterized
!                               derived type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type container (k, n)
        integer, kind :: k
        integer, len :: n

        type(base(k,n)) :: data(10)
    end type

    type(base(8,:)), allocatable :: b1_m(:)
end module

program dtparamConstr030d1
use m
    type (container(4,20)) :: co1

    allocate (base(8,20) :: b1_m(10))

    !! b1_m is not valid to be used for assignment to co1%data
    co1 = container(4,20)(data = b1_m)
end
