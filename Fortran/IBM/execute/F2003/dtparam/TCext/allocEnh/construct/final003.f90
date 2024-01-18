! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/construct/final003.f
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
!*  DATE                       : 11/09/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the finalization process during the
!                               intrinsic assignment for rank-two arrays.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        real(k1)      :: x, y

        contains

        final :: finalizePointRank2
    end type

    contains

    subroutine finalizePointRank2 (p1)
        type(point(*,4)), intent(inout) :: p1(:,:)

        print *, lbound(p1), '&', ubound(p1)

        p1%x = sqrt(-1.0)
        p1%y = sqrt(-1.0)
    end subroutine
end module


program final003
use m
    logical(4), external :: precision_r4

    type (point(:,4)), allocatable :: p1(:,:)

    type(point(20,4)) a2(12, 50)

    print *, 'start'

    !! test 1: 2 finalizations to happen: finalize a2(:, 1:25) before
    !assignment; then finalize the temporary created by reshape
    a2(:, 1:25) = reshape([(point(20,4)(i, i*.5), i=1,400)], [12,25])


    !! test 2: no finalization whatsoever
    print *, '2'

    a2(:,26:50)%x = reshape([(i, i=301,650)], [12,25])
    a2(:,26:50)%y = a2(:,26:50)%x/2.0

    !! test 3: initial allocation of p1, no finalization to be done
    print *, 3

    p1 = a2

    if (.not. allocated(p1)) error stop 1_4

    if (any(lbound(p1) /= 1) .or. any(ubound(p1) /= [12,50])) error stop 2_4

    k = 1

    do j = 1, 50
        do i = 1, 12
            if (.not. precision_r4(k*1.0_4, p1(i,j)%x)) error stop 3_4

            if (.not. precision_r4(k*.5_4, p1(i,j)%y)) error stop 4_4

            k = k + 1
        end do
    end do

    !! test 4: reallocation due to shape mis-match; finalization done during
    !deallocation
    print *, 4

    p1 = a2(:,1:40:2)

    if (any(lbound(p1) /= 1) .or. any(ubound(p1) /= [12,20])) error stop 5_4

    k = 1

    do j = 1, 20
        do i = 1, 12
            if (.not. precision_r4(k*1.0_4, p1(i,j)%x)) error stop 6_4

            if (.not. precision_r4(k*.5_4, p1(i,j)%y)) error stop 7_4

            k = k + 1
        end do

        k = K + 12
    end do

    !! test 5: no reallocation for allocatable variable; finalization done for
    !intrinsic assignment for p1
    print *, 5

    p1 = a2(12:1:-1,:20)

    k = 1

    do j = 1, 20
        do i = 12, 1, -1
            if (.not. precision_r4(k*1.0_4, p1(i,j)%x)) error stop 8_4

            if (.not. precision_r4(k*.5_4, p1(i,j)%y)) error stop 9_4

            k = k + 1
        end do
    end do


    !! test 6: finalization for section p1(:,::2)
    print *, 6

    p1(:,::2) = p1(:,2::2)

    k = 13

    do j = 2, 20, 2
        do i = 12, 1, -1
            if (.not. precision_r4(k*1.0_4, p1(i,j)%x)) error stop 10_4

            if (.not. precision_r4(k*.5_4, p1(i,j)%y)) error stop 11_4

            if (.not. precision_r4(k*1.0_4, p1(i, j-1)%x)) error stop 12_4

            if (.not. precision_r4(k*.5_4, p1(i, j-1)%y)) error stop 13_4
            k = k + 1
        end do

        k = k + 12
    end do

    print *, 'end'
end
