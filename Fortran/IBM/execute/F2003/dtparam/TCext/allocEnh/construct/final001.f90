! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/construct/final001.f
! opt variations: -qnol -qnodeferredlp

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
!*  DATE                       : 11/7/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the finalization process during the
!                               auto-reallocation process for the intrinsic
!                               assignment for allocatable enhancement.
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

        contains

        final :: finalizeBaseRank1
    end type

    contains

    subroutine finalizeBaseRank1 (b1)
        type(base(*,4)), intent(inout) :: b1(:)

        print *, 'finalizeBaseRank1'

        print *, lbound(b1,1), ubound(b1,1)

        do i = lbound(b1,1), ubound(b1,1)
            if (allocated(b1(i)%ids)) deallocate(b1(i)%ids)
        end do
    end subroutine
end module

program final001
use m
    type(base(:,4)), allocatable :: b1(:)

    !! there is no finalization at this point
    b1 = [(base(20,4)(null()), i=1,100)]

    if (size(b1) /= 100) error stop 1_4

    do i = 1, 100
        if (allocated(b1(i)%ids)) error stop 2_4
    end do

    !! finalization happens during deallocation of b1 (then reallocated)
    b1 = b1(21:30)

    if (size(b1) /= 10) error stop 3_4

    do i = 1, 10
        if (allocated(b1(i)%ids)) error stop 4_4
    end do

    !! finalization happens for the assignment
    b1 = [(base(20,4)([(j, j= 1, i)]), i=1,10)]

    if (size(b1) /= 10) error stop 5_4

    do i = 1, 10
        if (size(b1(i)%ids) /= i) error stop 6_4

        do j = 1, i
            if (b1(i)%ids(j) /= j) error stop 7_4
        end do
    end do
end
