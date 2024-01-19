! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-05
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : a test on matrix column access.
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
module arrays_mod
    implicit none
    integer, parameter :: m = 64, p = 128

    real, save :: array(m*p/4)[*] ! test > 4 images
end module

program test_matrix_col_access01
use my_matrix_mod
use arrays_mod
    implicit none

    integer me, np, i, k, img, lsz

    me = this_image()
    np = num_images()

    if (np < 4) then
        print *, 'at least 4 images needed'
        stop 10
    end if

    if (.not. is_dividable (m,p)) then
        print *, 'Error: data distribution issue'
        stop 20
    end if

    lsz = m*p/np

    array(:lsz) = [(k, k =(me-1)*lsz+1, me*lsz)]

    sync all

    call verify

    contains

    ! this routine treats the whole global array as a matrix of shape of (m*p/2, 2)
    subroutine verify
        real, allocatable :: local_arr(:)
        integer i, j
        logical, external :: precision_r4

        if (this_image() == 1) then
            local_arr = col(array, m*p/2, 2, 1)

            do i = 1, m*p/2
                if (.not. precision_r4(local_arr(i), 1.0*i)) then
                    print *, 'Validation failed for element', i, 'for 1st column'
                    print *, local_arr(i), 'vs', 1.0*i
                    error stop 1
                end if
            end do
        else if (this_image() == num_images()) then
            local_arr = col(array, m*p/2, 2, 2)

            do i = 1, m*p/2
                if (.not. precision_r4(local_arr(i), 1.0*(i+m*p/2))) then
                    print *, 'Validation failed for element', i, &
                            'for 2nd column'
                    print *, local_arr(i), 'vs', 1.0*(i+m*p/2)
                    error stop 1
                end if
            end do
        end if
    end subroutine
end
