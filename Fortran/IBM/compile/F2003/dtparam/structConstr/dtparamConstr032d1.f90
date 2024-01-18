!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/13/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Rank one array used for rank two array
!                               data component: parameterized derived type data.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data (n)
    end type

    type container (k, n, dim1, dim2)
        integer, kind :: k
        integer, len :: n, dim1, dim2

        type (base(k,n)) :: data(dim1, dim2)
    end type
end module

program dtparamConstr032d1
use m
    type (base(8,:)), allocatable :: b1(:)

    type (container(8,:,:,:)), allocatable :: co1

    allocate (container(8,25,15,35) :: co1)

    allocate (base(8,25) :: b1(15*35))

    co1 = container(8,25,15,35) (b1)
end
