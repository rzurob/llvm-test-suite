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
!*  DATE                       : 08/15/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               Use the structure constructor in an intrinsic
!                               assignment in the forall body.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k, dim)
        integer, kind :: k
        integer, len :: dim

        real(k) :: x(dim)
    end type

    type base (k)
        integer, kind :: k

        type(point(k,:)), allocatable :: data
    end type
end module

program dtparamConstr052
use m
    type(base(4)), allocatable :: b1(:)

    type (base(8)), pointer :: b2(:,:)

    logical(4), external :: precision_r4, precision_r8

    allocate (b1(100), b2(20, 10))

!    forall (i=10:50)
    do i = 10, 50
        b1(i) = base(4)(point(4,i)((/(j, j=i,2*i-1)/)))
    end do
!    end forall

!    forall (i=2:5, j=3:7)
    do i = 2, 5
        do j = 3, 7
            b2(i,j) = base(8)(point(8,2)(10*i+j))
        end do
    end do
!    end forall

    !! verify b1 and b2
    do i = 1, 9
        if (allocated(b1(i)%data)) error stop 1_4
    end do

    do i = 51, 100
        if (allocated(b1(i)%data)) error stop 2_4
    end do

    do i = 10, 50
        if (.not. allocated(b1(i)%data)) error stop 3_4

!        if (b1(i)%data%dim /= i) error stop 4_4
        if (size(b1(i)%data%x) /= i) error stop 4_4

        do j = i, i*2 - 1
            if (.not. precision_r4(b1(i)%data%x(j-i+1), j*1.0_4)) error stop 5_4
        end do
    end do

    do i = 1, 20
        do j = 1, 10
            if (((i <= 5) .and. (i >= 2)) .and. &
                ((j <= 7) .and. (j >= 3))) then
                if (.not. allocated(b2(i,j)%data)) error stop 6_4

                if (size(b2(i,j)%data%x) /= 2) error stop 7_4

                if (.not. precision_r8 (b2(i,j)%data%x(1), (10*i+j)*1.0_8)) &
                        error stop 8_4

                if (.not. precision_r8 (b2(i,j)%data%x(2), (10*i+j)*1.0_8)) &
                        error stop 9_4
            else
                if (allocated(b2(i,j)%data)) error stop 10_4
            end if
        end do
    end do
end
