! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/21/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the intrinsic in a do-loop and also involve
!                               the bound-remapping for the data pointer
!                               assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(4), allocatable :: data(:)
    end type
end module

program doloop003
use m
    type(base), allocatable, target :: b1(:)

    type(base), pointer :: b2(:,:,:)

    logical(4), external :: precision_r4

    b1 = (/(base(null()), i=1,100)/)

    b2(0:3, -1:3, 0:3) => b1(2:)

    do i = 0, 3
        do j = -1, 3
            do k = 0, 3
                b2(i,j,k)%data = (/(ip, ip=1,i+j+k)/)
            end do
        end do
    end do

    if (allocated(b1(1)%data)) error stop 1_4

    do i = 82, 100
        if (allocated(b1(i)%data)) error stop 2_4
    end do

    index1 = 2

    do k = 0, 3
        do j = -1, 3
            do i = 0, 3
                if ((.not. allocated(b1(index1)%data)) .or. &
                    (size(b1(index1)%data) /= max(0,i+j+k))) error stop 3_4

                do ip = 1, i+j+k
                    if (.not. precision_r4(b1(index1)%data(ip), ip*1.0_4)) &
                        error stop 4_4
                end do

                index1 = index1 + 1
            end do
        end do
    end do
end
