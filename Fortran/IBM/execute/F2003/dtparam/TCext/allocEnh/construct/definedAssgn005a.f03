! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/allocEnh/construct/definedAssgn005a.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/15/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               A similar case to definedAssgn005 except the
!                               allocatable component also contains allocatable
!                               component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind            :: k1
        integer(k1), allocatable :: id(:)

        integer(k1)              :: baseVal = 1000

        contains

        procedure :: assgn => assignB1B2
        generic :: assignment(=) => assgn
    end type


    contains

    elemental subroutine assignB1B2 (b1, b2)
        class(base(4)), intent(out) :: b1
        class(base(4)), intent(in) :: b2


        if (allocated(b2%id)) then
            b1%id = b2%id

            b1%id = b1%id + b1%baseVal
        end if

        b1%baseVal = b2%baseVal
    end subroutine
end module

module m1
use m
    type container(k2)    ! (4)
        integer, kind               :: k2
        type(base(k2)), allocatable :: data(:,:)
    end type
end module

program definedAssgn005a
use m1
    type(container(4)), allocatable :: co1

    type(base(4)) b1(0:1, 0:4)

    b1 = reshape((/(base(4)((/(j, j=1,i)/), 0), i=1,10)/), (/2,5/))

    co1 = container(4) (b1)


    if (.not. allocated(co1)) error stop 1_4

    if (.not. allocated(co1%data)) error stop 2_4

    if (any(lbound(co1%data) /= 0) .or. any(ubound(co1%data) /= (/1,4/))) &
        error stop 3_4

    k = 1

    do j = 0, 4
        do i = 0, 1

            if (.not. allocated(co1%data(i,j)%id)) error stop 4_4
            if (size(co1%data(i,j)%id) /= k) error stop 5_4

            do l = 1, k
                if (co1%data(i,j)%id(l) /= 2000 + l) error stop 6_4
            end do

            if (co1%data(i,j)%baseVal /= 0) error stop 8_4

            k = k + 1
        end do
    end do
end
