! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/construct/d327861.f
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
!*  DESCRIPTION                : miscellaneous (defect 327861)
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

use m
    type(base(:,4)), pointer :: b1(:)
    type(base(20,4)) b2(10)

    allocate(base(20,4) :: b1(10))

    do i = 1, 10
        b1(i)%ids = [integer :: ]

        b2(i)%ids = [(j, j=1, i)]
    end do

    b1 = b2

    if (.not. associated(b1)) stop 10

    do i = 1, 10
        if ((.not. allocated(b1(i)%ids) .or. (size(b1(i)%ids) /= i))) stop 1

        do j = 1, i
            if (b1(i)%ids(j) /= j) stop 3
        end do

        if (any(b2(i)%ids /= [(j, j=1,i)])) stop 2
    end do
end
