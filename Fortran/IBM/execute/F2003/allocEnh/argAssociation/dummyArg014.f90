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
!*  DATE                       : 10/31/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that the pointer component of a derived
!                               dummy argument with VALUE attribute used in the
!                               intrinsic assignment for an allocatable dummy
!                               argument.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: ids(:)
    end type

    type, extends(base) :: child
        character(:), allocatable :: name
    end type

    type container
        type(child), pointer :: data(:) => null()
    end type

    contains

    subroutine reAssgn (co1, c1)
        type(container), value :: co1
        type(child), allocatable :: c1(:)

        if (associated(co1%data)) c1 = co1%data
    end subroutine
end module

program dummyArg014
use m
    type(container), allocatable :: co1
    type(child), allocatable, target :: c1(:)

    co1 = container(null())

    allocate (co1%data(0:9))

    do i = 0, 9
        co1%data(i)%ids = [(j, j=1,i)]

        co1%data(i)%name = 'xlftest ' // achar(65+i)
    end do

    call reAssgn (co1, c1)

    if (.not. allocated(c1)) error stop 1_4

    if ((lbound(c1,1) /= 0) .or. (ubound(c1,1) /= 9)) error stop 2_4

    do i = 0, 9
        if (size(c1(i)%ids) /= i) error stop 3_4

        do j = 1, i
            if (c1(i)%ids(j) /= j) error stop 4_4
        end do

        if (c1(i)%name /= 'xlftest ' // achar(65+i)) error stop 5_4
    end do


    !! 2nd test
    call reAssgn (container(co1%data(9:0:-1)), c1)

    do i = 0, 9
        do j = 1, 9-i
            if (c1(i)%ids(j) /= j) error stop 6_4
        end do

        if (c1(i)%name /= 'xlftest ' // achar(74-i)) error stop 7_4
    end do


    !! 3rd test
    call reAssgn (container(c1(::2)), c1)

    if ((lbound(c1,1) /= 1) .or. (ubound(c1,1) /= 5)) error stop 8_4

    do i = 1, 5
        do j = 1, 11-2*i
            if (c1(i)%ids(j) /= j) error stop 9_4
        end do

        if (c1(i)%name /= 'xlftest ' // achar(76-2*i)) error stop 10_4
    end do
end
