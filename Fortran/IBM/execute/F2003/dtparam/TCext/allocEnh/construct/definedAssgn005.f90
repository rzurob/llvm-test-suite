! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/allocEnh/construct/definedAssgn005.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/15/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that intrinsic assignment for an
!                               allocatable of a derived type with allocatable
!                               component that is of another derived type with
!                               type-bound defined assignment; this is an array
!                               case to definedAssgn002.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id

        integer(k1)   :: baseVal = 1000

        contains

        procedure :: assgn => assignB1B2
        generic :: assignment(=) => assgn
    end type


    contains

    elemental subroutine assignB1B2 (b1, b2)
        class(base(*,4)), intent(out) :: b1
        class(base(*,4)), intent(in) :: b2


        b1%id = b2%id + b1%baseVal
        b1%baseVal = b2%baseVal
    end subroutine
end module

module m1
use m
    type container(k2,n2)    ! (4,20)
        integer, kind                 :: k2
        integer, len                  :: n2
        type(base(:,k2)), allocatable :: data(:,:)
    end type
end module

program definedAssgn005
use m1
    type(container(4,:)), allocatable :: co1, co2(:), co3(:)

    type(base(20,4)) b1(0:1, 0:4)

    b1 = reshape((/(base(20,4)(i, 0), i=1,10)/), (/2,5/))

    co1 = container(4,20) (b1)

    allocate (container(4,20) :: co3(0:9))

    do i = 0, 9
        allocate (co3(i)%data(0:4, 0:1), &
            source = reshape((/(base(20,4)(i), i=1,10)/), (/5,2/)))
    end do

    co2 = co3

    !! verify co1 and co2
    if (any(lbound(co1%data) /= 0) .or. any(ubound(co1%data) /= (/1,4/))) &
            error stop 1_4

    k = 1

    do j = 0, 4
        do i = 0, 1
            if ((co1%data(i,j)%id /= 2000 + k) .or. &
                (co1%data(i,j)%baseVal /= 0)) error stop 2_4

            k = k + 1
        end do
    end do

    if ((lbound(co2,1) /= 0) .or. (ubound(co2,1) /= 9)) error stop 3_4

    do i = 0, 9
        k = 1
        do j2 = 0,1
            do j1 = 0, 4
                if (co2(i)%data(j1,j2)%id-1000 /= k) error stop 4_4
                if (co2(i)%data(j1,j2)%baseVal /= 1000) error stop 5_4

                k = k + 1
            end do
        end do
    end do
end
