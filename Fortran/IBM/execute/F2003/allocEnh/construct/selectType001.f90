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
!*  DATE                       : 09/12/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the unlimited poly-entities being allocated
!                               allocatable intrinsic type array; and use this
!                               array in different assignment statements.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        complex, allocatable :: cx
    end type

    type (base), allocatable :: b1_m, b2_m(:)
end module

program selectType001
use m
    class(*), allocatable :: x1(:)
    logical(4), external :: precision_x8

    allocate(base :: x1(0:99))

    !! assign values to x1
    select type (x1)
        class is (base)
            do i = 0, 99
                x1(i)%cx = cmplx(i, i*2)
            end do

        class default
            stop 10
    end select


    !! assign x1's values to b1_m and b2_m
    select type (x1)
        type is (base)
            b1_m = x1(10)

            b2_m = x1

        class default
            stop 20
    end select

    if ((.not. allocated(b1_m)) .or. (.not. allocated(b2_m))) error stop 1_4

    if (.not. precision_x8(b1_m%cx, cmplx(10, 20,4))) error stop 2_4

    do i = 0, 99
        if (.not. precision_x8(b2_m(i)%cx, cmplx(i, i*2, 4))) error stop 3_4
    end do
end
