!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/11/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               More on use of expressions as the data source
!                               for allocatable component; involve intrinsic
!                               function.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len  :: n

        integer(k), allocatable :: ids(:,:)
        character(n), allocatable :: words(:,:)
    end type
end module

program dtparamConstr049a2
use m
    character(:), allocatable :: words(:,:)

    type(base(4,10)) b1

    integer i1(2,3)

    i1 = reshape((/1,2,3,4,5,6/), (/2,3/))

    words = reshape((/character(10) :: '12', '23', '34', '45', '56', '67'/), &
            (/2, 3/))

    b1 = base(4,10)(matmul(i1, transpose(i1)), transpose(words))

    if ((.not. allocated(b1%ids)) .or. (.not. allocated(b1%words))) &
            error stop 1_4

    if (any(shape(b1%ids) /= (/2,2/))) error stop 2_4
    if (any(shape(b1%words) /= (/3,2/))) error stop 3_4

    if (any (b1%ids(1,:) /= (/35, 44/)) .or.&
        any (b1%ids(2,:) /=(/44, 56/))) error stop 4_4

    if (any (b1%words(:,1) /= (/'12', '34', '56'/))) error stop 5_4

    if (any (b1%words(:,2) /= (/'23', '45', '67'/))) error stop 6_4
end
