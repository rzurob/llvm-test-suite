! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/allocEnh/construct/selectType003a.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=base

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/21/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               An array cas of selectType003.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,16)
        integer, kind         :: k1
        integer, len          :: n1
        real(k1), allocatable :: data(:)
    end type

    type, extends(base) :: child(k2,n2,k3)    ! (20,16,4,20,16)
        integer, kind                 :: k2,k3
        integer, len                  :: n2
        type(base(:,k3)), allocatable :: r1
    end type
end module

program selectType003a
use m
    class(base(:,16)), allocatable :: b1(:)

    logical(4), external :: precision_r6

    allocate(child(20,16,4,20,16) :: b1(0:99))

    select type (x => b1)
        type is (child(*,16,4,*,16))
            do i = 0, 99
                x(i)%data = (/(j, j=1,i)/)
                x(i)%r1 = base(20,16)(x(i)%data)
            end do
    end select

    !! verify results
    do i = 0, 99
        if ((.not. allocated(b1(i)%data)) .or. &
            (size(b1(i)%data) /= i)) error stop 1_4

        do j = 1, i
            if (.not. precision_r6(b1(i)%data(j), j*1.0_16)) error stop 2_4
        end do

        select type (x => b1(i))
            class is (child(*,16,4,*,16))
                if ((.not. allocated(x%r1)) .or. &
                    (.not. allocated(x%r1%data))) error stop 3_4

                if (size(x%r1%data) /= i) error stop 4_4

                do j = 1, i
                    if (.not. precision_r6(x%r1%data(j), b1(i)%data(j))) &
                        error stop 5_4
                end do

            class default
                error stop 6_4
        end select
    end do
end
