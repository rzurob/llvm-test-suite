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
!                               Test that intrinsic assignment for an
!                               allocatable of a derived type with allocatable
!                               component that is of another derived type with
!                               type-bound defined assignment; this is an array
!                               case to definedAssgn002.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id

        integer :: baseVal = 1000

        contains

        procedure :: assgn => assignB1B2
        generic :: assignment(=) => assgn
    end type


    contains

    elemental subroutine assignB1B2 (b1, b2)
        class(base), intent(out) :: b1
        class(base), intent(in) :: b2


        b1%id = b2%id + b1%baseVal
        b1%baseVal = b2%baseVal
    end subroutine
end module

module m1
use m
    type container
        type(base), allocatable :: data(:,:)
    end type
end module

program definedAssgn005
use m1
    type(container), allocatable :: co1, co2(:), co3(:)

    type(base) b1(0:1, 0:4)

    b1 = reshape((/(base(i, 0), i=1,10)/), (/2,5/))

    co1 = container (b1)

    allocate (co3(0:9))

    do i = 0, 9
        allocate (co3(i)%data(0:4, 0:1), &
            source = reshape((/(base(i), i=1,10)/), (/5,2/)))
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
