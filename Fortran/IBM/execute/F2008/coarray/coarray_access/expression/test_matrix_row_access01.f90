! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-12
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : a simple test on the matrix date access by rows
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
module matrix_op_mod
    use my_matrix_mod, only: set_col_val, row
    implicit none
    real, save :: mat1(32*1024)[*]
    integer, parameter :: m = 32, n = 64*32 ! used as the shape of the matrix

    type matrix_op
        real, allocatable :: row_data(:)

        contains

        procedure, nopass :: setup => initiate_matrix
        procedure, pass :: fill_one_row
    end type

    contains

    subroutine fill_one_row (m1, row_num)
        class(matrix_op), intent(out) :: m1
        integer, intent(in) :: row_num

        m1%row_data = row (mat1, m, n, row_num)
    end subroutine

    ! set up the matrix to be with contiguous values in virtual element order
    subroutine initiate_matrix
        integer :: me, np, cols_per_image
        integer i, col_num, k

        np = num_images()
        me = this_image()

        if (mod(n, np) /= 0) stop 10
        cols_per_image = n/np


        ! set up the values of the matrix, the shape of the matrix is 32 by 2048
        do i = 1, cols_per_image
            col_num = (me-1)*cols_per_image + i

            call set_col_val (mat1,[m,n], &
                [((col_num-1)*m +k, k = 1, m)]*1.0, col_num)
        end do
        sync all
    end subroutine
end module

program test_matrix_row_access
use matrix_op_mod, only: matrix_op, m,n
    implicit none

    integer me, np, i, j
    type(matrix_op), pointer :: op
    logical, external :: precision_r4

    np = num_images()
    me = this_image()


    if (np < 2) then
        print *, 'this program requires at least two images to run'
        stop 1
    end if

    if (np > n) stop 2

    allocate (op)

    call op%setup

    ! verify the results on one image
    if (me == np) then
        do i = 1, m
            call op%fill_one_row (i)

            do j = 1, n
                if (.not. precision_r4(op%row_data(j), 1.0*(i+(j-1)*m))) then
                    print *, 'on row',i,': failed to verify column', j
                    print *, op%row_data(j), 'vs', 1.0*(i+(j-1)*m)
                    error stop 1
                end if
            end do
        end do
    end if
end
