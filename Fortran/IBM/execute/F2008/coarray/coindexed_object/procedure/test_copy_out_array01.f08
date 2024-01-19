! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-11-09
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : a simple test to verify that the coindexed
!                               array objects used as actual argument are copied
!                               back to remote place. Note this is not a
!                               recommended way of passing data.
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
    complex(8), save :: array(10,5)[*]

    contains

    subroutine set_val (x, vals, shapes)
        integer, intent(in) :: shapes(2)
        complex(8), intent(inout) :: x(shapes(1), shapes(2))
        complex(8), intent(in) :: vals(shapes(1), shapes(2))

        x = vals
    end subroutine
end module

program test_copy_out_array01
use data_mod
    implicit none

    integer i, j, k
    integer me, np, left

    complex(8), allocatable :: temp_array(:)
    logical, external :: precision_x6

    np = num_images()
    me = this_image()

    if (me == 1) then
        left = np
    else
        left = me - 1
    end if

    allocate (temp_array(size(array)))
    k = 1

    do j = 1, size(array, 2)
        do i = 1, size(array, 1)
            temp_array(k) = left * cmplx(1.0_8*i, j*1.0_8, 8)
            k = k + 1
        end do
    end do

    call set_val (array(:,:)[left], temp_array, shape(array))
    sync all

    do j = 1, size(array, 2)
        do i = 1, size(array, 1)
            if (.not. precision_x6(array(i,j), me*cmplx(1.0_8*i, j*1.0_8, 8))) then
                print *, 'failed to verify array on image',me,'for element',i,j
                print *, array(i,j), 'vs', me*cmplx(1.0_8*i, j*1.0_8, 8)
                error stop 1
            end if
        end do
    end do
end
