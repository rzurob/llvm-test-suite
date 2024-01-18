! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : a test that verifies that scalar coindexed
!                               objects are coopied out.  Test use a scalar
!                               dummy with array elements as the coindexed
!                               object actual.
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
    implicit none
    real, save :: arr1(10, 5)[*] = 0
end module

module op_mod
    implicit none

    contains

    subroutine set_data(x, i, j, factor)
        real x
        integer, intent(in) :: i, j
        real, intent(in) :: factor

        x = 10*j + i

        x = x * factor
    end subroutine
end module

program test_copy_out_scalar02
use data_mod, only : arr1
use op_mod, only : set_data
    implicit none

    integer i, j, me, np, left

    np = num_images()
    me = this_image()

    if (me == 1) then
        left = np
    else
        left = me - 1
    end if

    do j = 1, 5
        do i = 1, 10
            call set_data(arr1(i, j)[left], i, j, 1.0*left)
        end do
    end do

    call verify_all

    contains

    subroutine verify_all
        logical, external :: precision_r4
        sync all

        do i = 1, 10
            do j = 1, 5
                if (.not. precision_r4 (arr1(i, j), 1.0*me*(10*j+i))) then
                    print *, 'Failed to verify arr1 on image', me,&
                        'for element', i, 'and', j
                    print *, arr1(i, j), 'vs', 1.0*(10*j+i)*me
                    error stop 1
                end if
            end do
        end do
    end subroutine
end
