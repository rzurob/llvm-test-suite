! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2011-01-10
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : test assignment on complex(4) data type.
!                                test a rank-two array.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
module data_mod
    complex, save :: cx1(5, 6)[*]
end module

program assign_cmplx2
    use data_mod
    implicit none

    complex, save :: cx2(5,5)[*]
    integer :: me, np, i, j, k
    integer :: left, right

    np = num_images()
    me = this_image()

    if (np < 2) then
        print *, 'Error: requires at least two images to run the program'
        error stop 1
    end if

    if (me == 1) then
        left = np
    else
        left = me - 1
    end if

    if (me == np) then
        right = 1
    else
        right = me + 1
    end if

    !! test that coindexed scalar used in assignment as LHS
    do j = 1, 5
        do i = 1, 5
            cx2(i,j)[left] = cmplx(i, j)*left
        end do
    end do
    sync all

    cx1(:,1:5)[right] = cx2(:,:)[right]

    cx1(:,6)[left] = [(cx2(i,i)[left], i = 1, 5)]
    sync all

    call verify_cmplx4

    contains

    ! this routine verifies cx1 and cx2
    subroutine verify_cmplx4
        integer :: i, j
        logical, external :: precision_x8

        do i = 1, 5
            do j = 1, 5
                if (.not. precision_x8(cx2(i,j), cmplx(i,j)*me)) then
                    print *, 'Error: verify cx2 failed on image', me
                    print *, 'element:',i,',',j,'Expected: ',cmplx(i,j)*me, &
                             '; Actual: ',cx2(i,j)

                    error stop 1
                end if

                if (.not. precision_x8(cx1(i,j), cmplx(i,j)*me)) then
                    print *, 'Error: verify cx1 failed on image', me
                    print *, 'element:',i,',',j,'Expected: ',cmplx(i,j)*me, &
                             '; Actual: ',cx1(i,j)

                    error stop 1
                end if
            end do
        end do

        do i = 1, 5
            if (.not. precision_x8(cx1(i, 6), cmplx(i,i)*me)) then
                print *, 'Error: verify cx1 failed on image', me
                print *, 'element:',i,',6', 'Expected: ',cmplx(i,j)*me, &
                        '; Actual: ',cx1(i,6)

                error stop 1
            end if
        end do
    end subroutine
end
