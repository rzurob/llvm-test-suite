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
!*  DATE                       : 10/24/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the case where expr is a scalar while var
!                               is an array: in this case, there is no
!                               reallocation for var as if an array of expr as
!                               the same shape as var appears as the RHS.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        complex(8) cx(2)
    end type
end module

program zeroSizeArray012
use m
    type(base), allocatable :: b1(:,:)

    logical(4), external :: precision_x6

    b1 = reshape([(base(cmplx([i,i+1], [i, i+1], 8)), i=1,20)], [4,5])

    if (any(shape(b1) /= [4,5])) error stop 1_4

    k = 1

    do j = 1, 5
        do i = 1, 4
            if (.not. precision_x6(b1(i,j)%cx(1), cmplx(k,k,8))) error stop 2_4

            if (.not. precision_x6(b1(i,j)%cx(2), cmplx(k+1,k+1,8))) &
                error stop 3_4

            k = k + 1
        end do
    end do

    !! assign the entire array to a single value
    b1 = b1(4,4)

    if (any(shape(b1) /= [4,5])) error stop 4_4

    do j = 1, 5
        do i = 1, 4
            if (.not. precision_x6(b1(i,j)%cx(1), cmplx(16,16,8))) &
                error stop 5_4

            if (.not. precision_x6(b1(i,j)%cx(2), cmplx(17,17,8))) &
                error stop 6_4
        end do
    end do


    !! set to zeroSized array
    i = 10

    b1 = reshape([base :: ], [i, i-10])

    b1 = base([1,2])

    if (any(shape(b1) /= [10, 0])) error stop 7_4
end
