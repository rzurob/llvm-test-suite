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
!                               Similar to definedAssgn004 except that the
!                               component that has the type-bound defined
!                               assignment is not allocatable.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real, allocatable :: data(:)

        contains

        procedure :: assgn => assgnB1B2
        generic :: assignment(=) => assgn
    end type

    contains

    subroutine assgnB1B2 (b1, b2)
        class(base), intent(out) :: b1
        class(base), intent(in) :: b2

        if (allocated(b2%data)) then
            b1%data = b2%data

            b1%data = -b2%data
        end if
    end subroutine
end module

module m1
use m
    type container
        type (base) :: data
    end type
end module

program definedAssgn004a
use m1
    type (container), allocatable :: co1, co2(:), co3(:)

    real r1(0:9)

    logical(4), external :: precision_r4

    r1 = (/(i, i=1,10)/)

    allocate (co3(0:9))

    do i = 0, 9, 2
        co3(i)%data = base(null())

        co3(i+1)%data = base(r1)
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
