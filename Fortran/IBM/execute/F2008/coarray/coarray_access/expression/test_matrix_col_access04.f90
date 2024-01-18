! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-10-12
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : a test on algrithm on assigning values based on
!                               column values.
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
module array_def_mod
    implicit none
    integer, parameter :: m = 1024, n = 1024*16
    real, save :: arr1(m*n/4)[*] !<-- this requires a minimum of 4 images to run
end module

program test_matrix_col_access04
use array_def_mod
use my_matrix_mod
    implicit none
    integer me, np, cols_per_image, i, col_no, k, j, l
    logical, external :: precision_r4
    real, pointer :: local_arr(:)

    me = this_image()
    np = num_images()

    if (np < 4) then
        print *, 'test_matrix_col_access04: Error: at least 4 images required'
        stop 1
    end if

    cols_per_image = n/np

    ! all images needs concurrently update the columns
    ! shouldn't have race conditions
    do i = 1, cols_per_image
        col_no = (me - 1)*cols_per_image + i
        call set_col_val(arr1, [m,n], [((col_no - 1)*m + k, k = 1, m)]*1.0, col_no)
    end do

    sync all

    ! verify results on image 1
    ! the results are as if a (m X n) matrix is built with contiguous values for
    ! each element
    if (me == 1) then
        k = 1
        allocate (local_arr(m*cols_per_image))

        do i = 1, np
            local_arr(:) = arr1(:m*cols_per_image)[i]

            do j = 1, cols_per_image
                do l = 1, m
                    if (.not. precision_r4(local_arr(l + (j-1)*m), k*1.0)) then
                        print *, 'validation fails on image', i, 'for element', k
                        print *, arr1(l + (j-1)*m)[i], 'vs', k*1.0
                        error stop 1
                    end if
                    k = k + 1
                end do
            end do
print *, 'done with', i
        end do

        deallocate (local_arr)
    end if
end
