! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-05
!*  ORIGIN                     :
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
    integer, parameter :: m = 64, n = 32, p = 128

    real, save :: array(m*n/4)[*] ! test > 4 images
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

    if (.not. is_dividable (m,n)) then
        print *, 'Error: data distribution issue'
        stop 20
    end if

    lsz = m*n/np

    if (me == np) then
        k = 1
        do img = 1, np
            do i = 1, lsz
                array(i)[img] = k

                k = k + 1
            end do
        end do
    end if
    sync all

    call verify

    contains

    subroutine verify
        real local_arr(m)
        integer num_images, i, j
        logical, external :: precision_r4

        num_images = n/np

        do i = (me-1)*num_images + 1, me*num_images
            local_arr = col (array, m, n, i)

            do j = 1, m
                if (.not. precision_r4(local_arr(j), 1.0*((i-1)*m+j))) then
                    print *, 'validation failed for column', i, 'element', j
                    print *, local_arr(j), 'vs', 1.0*((i-1)*m+j)
                    error stop 1
                end if
            end do
        end do
        print *, 'all done'
    end subroutine
end
