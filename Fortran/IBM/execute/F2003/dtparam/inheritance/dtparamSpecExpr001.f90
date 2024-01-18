!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/13/2005
!*
!*  DESCRIPTION                : dtparam (specification expression)
!                               Case: length type parameters used as the
!                               specification expression in the derived type
!                               definition.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (l1, l2, k)
        integer, kind:: k
        integer, len :: l1, l2

        integer(kind=k) data(min(l1,l2): max(l1, l2))
        character(len=l1+l2) name
    end type

    type (base(k=8, l1 =10, l2=20)) b1_m(2)

    integer(8), parameter :: i4Bound = 2_8**32_8
end module

program dtparamSpecExpr001
use m
    type (base(10, 6, 4)) b1(2,2)

    !! set the values for b1 and b1_m
    b1_m%name = (/'b1_m 1', 'b1_m 2'/)
    b1%name = reshape((/'b1 11', 'b1 21', 'b1 12', 'b1 22'/), (/2, 2/))

    b1_m(1)%data = (/(i4Bound+i, i=10,20)/)
    b1_m(2)%data = (/(i*10, i=10, 20)/)

    b1(1,1)%data = (/(i, i=10,6,-1)/)
    b1(2,1)%data = (/(i, i=6,10)/)
    b1(1,2)%data = b1(1,1)%data + b1(2,1)%data
    b1(2,2)%data = -100

    !! print the data
    print *, b1_m(1)
    print *, b1_m(2)

    do j = 1, 2
        do i = 1, 2
            print *, b1(i,j)
        end do
    end do
end
