! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/allocEnh/argAssociation/dummyArg014.f
! opt variations: -qnock -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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
    type base(n1,k1)    ! (20,4)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1), allocatable :: ids(:)
    end type

    type, extends(base) :: child    ! (20,4)
        character(:), allocatable :: name
    end type

    type container(k2,n2)    ! (4,20)
        integer, kind              :: k2
        integer, len               :: n2
        type(child(:,k2)), pointer :: data(:) => null()
    end type

    contains

    subroutine reAssgn (co1, c1)
        type(container(4,20)), value :: co1
        type(child(:,4)), allocatable :: c1(:)

        if (associated(co1%data)) c1 = co1%data
    end subroutine
end module

program dummyArg014
use m
    type(container(4,:)), allocatable :: co1
    type(child(:,4)), allocatable, target :: c1(:), c12(:)

    co1 = container(4,20)(null())

    allocate (child(20,4) :: co1%data(0:9))

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
    call reAssgn (container(4,20)(co1%data(9:0:-1)), c1)

    do i = 0, 9
        do j = 1, 9-i
            if (c1(i)%ids(j) /= j) error stop 6_4
        end do

        if (c1(i)%name /= 'xlftest ' // achar(74-i)) error stop 7_4
    end do


    !! 3rd test
    c12 = c1(::2)

    call reAssgn (container(4,20)(c12), c1)

    if ((lbound(c1,1) /= 1) .or. (ubound(c1,1) /= 5)) error stop 8_4

    do i = 1, 5
        do j = 1, 11-2*i
            if (c1(i)%ids(j) /= j) error stop 9_4
        end do

        if (c1(i)%name /= 'xlftest ' // achar(76-2*i)) error stop 10_4
    end do
end
