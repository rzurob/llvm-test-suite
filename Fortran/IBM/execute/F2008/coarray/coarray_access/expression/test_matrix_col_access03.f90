! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       :
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : similar test to test_matrix_col_access01
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                :
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
program test_matrix_col_access03
use my_matrix_mod
    implicit none
    integer, parameter :: m = 64, n = 1024*1024*8
    real, save :: array(n/4)[*] ! at least 4 images needed

    integer me, np, lsz, k

    me = this_image()
    np = num_images()

    if (np < 4) then
        print *, 'at  least 4 images needed'
        stop 10
    end if

    if (.not. is_dividable (m,n)) then
        print *, 'Error: data distribution issue'
        stop 20
    end if

    lsz = n/np

    array(:lsz) = [(k, k =(me-1)*lsz+1, me*lsz)]
    sync all

    call verifyarray

    contains

    subroutine verifyarray
        real, pointer :: local(:) => null()

        integer i, j
        logical, external :: precision_r4

        ! validate in parallel on a matrix (40, 1024*256)
        do i = 1, n/np/m
            if (.not. associated(local)) allocate (local(m))

            local = col(array, m, n/m, i + (me-1)*n/np/m)

            do j = 1, m
                if (.not. precision_r4 (local(j), &
                    (((i + (me-1)*n/np/m) -1)*m + j)*1.0)) then

                    print *, 'validation failed', j
                    print *, local(j), 'vs', &
                        (((i + (me-1)*n/np/m) -1)*m + j)*1.0

                    error stop 1
                end if
            end do
        end do

        deallocate (local)
    end subroutine
end
