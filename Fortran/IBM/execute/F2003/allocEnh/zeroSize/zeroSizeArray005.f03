! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/29/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Use the different intrinsic types in intrinsic
!                               assignments in a forall construct; for the
!                               components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        complex(8), allocatable :: cx(:,:)
    end type
end module

use m
    class(base), allocatable :: b1(:)
    integer, allocatable :: i1(:,:)

    i1 = reshape((/1.2/), (/2, 0/))

    allocate(b1(100))

    forall (i=1:100, mod(i,3)==0)
        b1(i)%cx = i1
    end forall

    !! verify the results of b1
    do i = 1, 100
        if (mod(i,3) == 0) then
            if (.not. allocated(b1(i)%cx)) error stop 1_4

            if (size(b1(i)%cx) /= 0) error stop 2_4
        else
            if (allocated(b1(i)%cx)) error stop 3_4
        end if
    end do
end
