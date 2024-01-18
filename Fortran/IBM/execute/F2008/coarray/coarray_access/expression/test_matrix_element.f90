! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-15
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : a test on set array element using my_matrix_mod
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
    integer, parameter :: m = 1024, n = 1024*8

    real, save :: arr(m*n/4)[*] !<-- requires at least 4 images to run
end module

program test_matrix_element
use my_matrix_mod, only: set_val, is_dividable
use data_mod
    implicit none

    integer me, np, i, j, k

    me = this_image()
    np = num_images()

    if (np < 4) then
        print *, 'this program requires at least 4 images to run'
        stop 10
    end if

    if ((np > n) .or.( .not. is_dividable(m,n))) then
        print *, 'the number of images',np, &
            'is not suitable for running the program'
        stop 20
    end if

    ! set the matrix values on the last image
    ! Note this is a very expensive way to use this call: a lot of network
    ! traffic is expected from the calls
    if (me == np) then
        k = 1
        do j = 1, n
            do i = 1, m
                call set_val (arr, m,n, [i,j], k*1.0)
                k = k + 1
            end do
        end do
    end if

    sync all

    ! verify the results on ALL images asynchronously
    call verify_results

    contains

    subroutine verify_results
        integer cols_per_imag
        integer, allocatable :: my_cols(:)
        logical, external :: precision_r4

        cols_per_imag = n/np

        my_cols = [((me-1)*cols_per_imag + i, i = 1, cols_per_imag)]

        do k = 1, cols_per_imag
            do i = 1+(k-1)*m, k*m
                if (.not. precision_r4(arr(i), 1.0*(i+(me-1)*cols_per_imag*m))) then
                    print *, 'Failed to verify on image', me, 'for element',i
                    print *, arr(i), 'vs',  1.0*(i+(me-1)*cols_per_imag*m)
                    error stop 1
                end if
            end do
        end do
    end subroutine
end
