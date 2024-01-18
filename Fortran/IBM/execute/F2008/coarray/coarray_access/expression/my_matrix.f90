! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-05
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : define a module that access a matrix via column
!                               or row
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
module my_matrix_mod
    implicit none
    contains

    logical function is_dividable (m, n)
        integer, intent(in) :: m, n

        integer np
        np = num_images()

        is_dividable = (mod(m*n, np) == 0)
    end function

    ! this function returns a column of a distributed matrix of shape (m x n)
    function col (x, m, n, num)
        integer, intent(in) :: m, n, num
        real, intent(in) :: x(*)[*]

        real col(m)

        integer me, np, local_size
        integer begin_img, end_img
        integer begin_index, end_index, i, k

        if ((num > n) .or. (num <= 0)) then
            print *, 'col():ERROR: invalid input'
            error stop 1
        end if

        me = this_image()
        np = num_images()

        local_size = m * n /np

        !compute the location of column num and construct the column
        ! by pasting sections
        if (mod(((num-1) * m + 1), local_size) == 0) then ! last element
            begin_img = ((num-1) * m + 1)/local_size
            begin_index = local_size
        else
            begin_img = ((num-1) * m + 1)/local_size + 1
            begin_index = mod(((num-1) * m + 1), local_size)
        end if

        if (mod(num*m, local_size) == 0) then
            end_img = num*m / local_size
            end_index = local_size
        else
            end_img = num*m / local_size + 1
            end_index = mod(num*m, local_size)
        end if

        if (begin_img == end_img) then ! we end up with only one image
            col = x(begin_index:end_index)[begin_img]
        else
            k = local_size - begin_index + 1

            col(:k) = x(begin_index:local_size)[begin_img]

            do i = begin_img+1, end_img-1
                col(k+1:k+local_size) = x(:local_size)[i]
                k = k + local_size
            end do

            col(k+1:) = x(:end_index)[end_img]
        end if
    end function

    ! set the values of a column in a matrix using an input array
    subroutine set_col_val (x, shape, val, col)
        integer, intent(in) :: shape(2), col
        real, intent(inout) :: x(*)[*]  ! the matrix
        real, intent(in) :: val(shape(1))

        integer me, np, local_size
        integer begin_img, end_img
        integer begin_index, end_index, i, k

        if ((col > shape(2)) .or. (col < 1)) then
            print *, 'set_col_val():ERROR: invalid input'
            error stop 1
        end if

        me = this_image()
        np = num_images()

        local_size = product(shape) /np

        !compute the location of column col and construct the column
        ! by pasting sections
        if (mod(((col-1) * shape(1) + 1), local_size) == 0) then ! last element
            begin_img = ((col-1) * shape(1) + 1)/local_size
            begin_index = local_size
        else
            begin_img = ((col-1) * shape(1) + 1)/local_size + 1
            begin_index = mod(((col-1) * shape(1) + 1), local_size)
        end if

        if (mod(col*shape(1), local_size) == 0) then
            end_img = col*shape(1) / local_size
            end_index = local_size
        else
            end_img = col*shape(1) / local_size + 1
            end_index = mod(col*shape(1), local_size)
        end if

        if (begin_img == end_img) then ! we end up with only one image
            x(begin_index:end_index)[begin_img] = val
        else
            k = local_size - begin_index + 1

            x(begin_index:local_size)[begin_img] = val(:k)

            do i = begin_img+1, end_img-1
                x(:local_size)[i] = val(k+1:k+local_size)
                k = k + local_size
            end do

            x(:end_index)[end_img] = val(k+1:)
        end if
    end subroutine

    function row (x, m, n, row_num) result(res)
        integer, intent(in) :: m, n, row_num
        real, intent(in), dimension(*), codimension[*] :: x
        real res(n)
        integer i, me, np, cols_per_image, k

        if ((row_num < 1) .or. (row_num > m)) then
            print *, 'Error: row number', row_num, 'is out of range'
            print *, 'should be between 1 and', m
            error stop 1
        end if

        me = this_image()
        np = num_images()

        cols_per_image = n/np

        k = 1
        do i = 1, np
            res(k:k+cols_per_image-1) = x(row_num : m*cols_per_image : m)[i]

            k = k + cols_per_image
        end do
    end function

    ! this subroutine sets the value for a particular indices in a matrix of
    ! shape (m, n)
    subroutine set_val (x, m, n, ind, val)
        integer, intent(in) :: m, n, ind(2)
        real, intent(inout) :: x(*)[*]
        real, intent(in) :: val

        integer index_val, img_index, np, cols_per_image

        if ((ind(1) > m) .or. (ind(2) > n)) then
            print *, 'set_val(): wrong index values of', ind
            error stop 1
        end if

        np = num_images()
        cols_per_image = n/np

        img_index = (ind(2)-1)/cols_per_image + 1

        ! compute the index value on the image img_index
        index_val = ind(1) + (ind(2)-1)*m - m*cols_per_image*(img_index-1)

        x(index_val)[img_index] = val
    end subroutine
end module

