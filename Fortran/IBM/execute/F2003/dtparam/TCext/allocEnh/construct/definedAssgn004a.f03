! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/allocEnh/construct/definedAssgn004a.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/15/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Similar to definedAssgn004 except that the
!                               component that has the type-bound defined
!                               assignment is not allocatable.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind         :: k1
        integer, len          :: n1
        real(k1), allocatable :: data(:)

        contains

        procedure :: assgn => assgnB1B2
        generic :: assignment(=) => assgn
    end type

    contains

    subroutine assgnB1B2 (b1, b2)
        class(base(*,4)), intent(out) :: b1
        class(base(*,4)), intent(in) :: b2

        if (allocated(b2%data)) then
            b1%data = b2%data

            b1%data = -b2%data
        end if
    end subroutine
end module

module m1
use m
    type container(k2,n2)    ! (4,20)
        integer, kind     :: k2
        integer, len      :: n2
        type(base(n2,k2)) :: data
    end type
end module

program definedAssgn004a
use m1
    type (container(4,:)), allocatable :: co1, co2(:), co3(:)

    real r1(0:9)

    logical(4), external :: precision_r4

    r1 = (/(i, i=1,10)/)

    allocate (container(4,20) :: co3(0:9))

    do i = 0, 9, 2
        co3(i)%data = base(20,4)(null())

        co3(i+1)%data = base(20,4)(r1)
    end do

    co2 = co3

    do i = 0, 9, 2
        if (allocated(co2(i)%data%data)) error stop 1_4

        do j = 0, 9
            if (.not. precision_r4(co2(i+1)%data%data(j), 1.0_4*(j+1))) &
                error stop 2_4
        end do
    end do
end
