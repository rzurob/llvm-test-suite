! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/allocEnh/construct/doloop001.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/11/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Use the intrinsic assignment for the allocatable
!                               component in nested do loops.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A(k1)    ! (4)
        integer, kind         :: k1
        real(k1), allocatable :: data(:)
    end type

    type base(k2)    ! (4)
        integer, kind            :: k2
        type(A(k2)), allocatable :: data(:,:)
    end type
end module

program doloop001
use m
    type(base(4)), allocatable :: b1(:)
    logical(4), external :: precision_r4

    b1 = (/(base(4)(null()), i=0,9)/)

    do i = 1, 10
        b1(i)%data = reshape((/(A(4)(null()), ip=0,9)/), (/2, 5/))

        do j = 1, 2
            do k = 1, 5
                b1(i)%data(j,k)%data = (/(ij, ij=1, i*100+j*10+k)/)
            end do
        end do
    end do

    !! verify results
    do i = 1, 10
        do j = 1, 2
            do k = 1, 5
                do l = 1, i*100+j*10+k
                    if (.not. precision_r4(b1(i)%data(j,k)%data(l), l*1.0)) &
                        error stop 1_4
                end do
            end do
        end do
    end do
end
