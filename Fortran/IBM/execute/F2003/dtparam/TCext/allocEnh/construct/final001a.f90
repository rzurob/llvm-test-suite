! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/construct/final001a.f
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
!*  DATE                       : 11/8/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the finalization process during the
!                               intrinsic assignment for array sections of
!                               rank-one.
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

program final001a
use m
    type(base(:,4)), allocatable :: b1(:)

    !! there is no finalization at this point
    b1 = [(base(20,4)([(j, j=1,i)]), i=1,100)]

    if (size(b1) /= 100) error stop 1_4

    print *, '1'
    !! finalization happens during the intrinsic assignment
    b1(100:1:-2) = b1(::2)

    print *, 2

    do i = 1, 100, 2
        if (size(b1(i)%ids) /= i) error stop 2_4

        do j = 1, i
            if (b1(i)%ids(j) /= j) error stop 3_4
        end do

        if (size(b1(i+1)%ids) /= 100-i) error stop 4_4

        do j = 1, 100 - i
            if (b1(i+1)%ids(j) /= j) error stop 5_4
        end do
    end do
end
