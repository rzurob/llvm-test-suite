! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/allocEnh/construct/definedAssgn004.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 09/15/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that the type-bound defined assignment
!                               applies to an allocatable component; allocatable
!                               scalar component.
!*
!*
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

    elemental subroutine assgnB1B2 (b1, b2)
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
        integer, kind                 :: k2
        integer, len                  :: n2
        type(base(:,k2)), allocatable :: data
    end type
end module

program definedAssgn004
use m1
    type (container(4,:)), allocatable :: co1, co2(:), co3(:)
    real r1(0:9)

    logical(4), external :: precision_r4

    r1 = (/(i, i=1,10)/)
    co1 = container(4,20)(base(20,4)(r1))

    allocate (container(4,20) :: co3(0:9))

    do i = 0, 9, 2
        allocate(base(20,4) :: co3(i)%data, co3(i+1)%data)

        co3(i+1)%data = base(20,4)(r1)
    end do

    co2 = co3

    if ((.not. allocated(co1)) .or. (.not. allocated(co2))) error stop 1_4

    if ((lbound(co2,1) /= 0) .or. (ubound(co2,1) /= 9)) error stop 2_4

    if ((lbound(co1%data%data,1) /= 0) .or. &
        (ubound(co1%data%data,1) /= 9)) error stop 3_4

    !! verify co1 values
    do i = 0, 9
        if (.not. precision_r4(co1%data%data(i), -1.0_4*(i+1))) &
            error stop 4_4
    end do

    !! verify co2 values
    do i = 0, 9, 2
        if (allocated(co2(i)%data%data)) error stop 5_4

        if (.not. allocated(co2(i+1)%data%data)) error stop 6_4

        if ((lbound(co2(i+1)%data%data,1) /= 0) .or. &
            (ubound(co2(i+1)%data%data,1) /= 9)) error stop 7_4

        do j = 0, 9
            if (.not. precision_r4 (co2(i+1)%data%data(j), 1.0_4*(j+1))) &
                error stop 8_4
        end do
    end do
end
