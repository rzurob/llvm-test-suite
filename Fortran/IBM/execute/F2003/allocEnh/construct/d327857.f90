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
!                               finalization for intrinsic assignment for
!                               allocatable variables.  (defect 327857)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: ids(:)

        contains

        final :: finalizeBaseRank1
    end type

    contains

    subroutine finalizeBaseRank1 (b1)
        type(base), intent(inout) :: b1(:)

        print *, 'finalizeBaseRank1'

        print *, lbound(b1,1), ubound(b1,1)

        do i = lbound(b1,1), ubound(b1,1)
            if (allocated(b1(i)%ids)) deallocate(b1(i)%ids)
        end do
    end subroutine
end module

use m
    type(base), allocatable :: b1(:)
    type(base) b2(10)

    allocate(b1(10))

    do i = 1, 10
        b1(i)%ids = [integer :: ]

        b2(i)%ids = [(j, j=1, i)]
    end do

    b1 = b2

    do i = 1, 10
        if ((.not. allocated(b1(i)%ids) .or. (size(b1(i)%ids) /= i))) stop 1

        do j = 1, i
            if (b1(i)%ids(j) /= j) stop 3
        end do

        if (any(b2(i)%ids /= [(j, j=1,i)])) stop 2
    end do
end
