! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/construct/doloop002.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/2/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test intrinsic assignments in do construct; some
!                               of the assignments will involve reallocations.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind         :: k1
        integer, len          :: n1
        real(k1), allocatable :: r1(:)
    end type

    contains

    integer function compute (b)
        type(base(*,4)), intent(in) :: b

        allocatable compute

        if (allocated(b%r1)) then
            compute = size(b%r1)
        else
            compute = 0
        end if
    end function
end module

use m
    type (base(:,4)), allocatable :: b1, b2(:)

    logical(4), external :: precision_r4

    b1 = base(20,4)((/real :: (j,j=1,10)/))

    b2 = [base(20,4) :: (base(20,4)(null()), i=1,compute(b1))]

    do i = 1, compute(b1)
        b2(i)%r1 = [real :: compute (b2(i))]

        do j = compute(b2(i)), i
            b2(i)%r1 = [real :: compute(b2(i)), b2(i)%r1]
        end do
    end do

    do i = 1, 10
        do j = i, 0, -1
            if (.not. precision_r4 (b2(i)%r1(i-j+1), j*1.0)) error stop 1_4
        end do

        if (.not. precision_r4 (b1%r1(i), i*1.0)) error stop 2_4
    end do
end
