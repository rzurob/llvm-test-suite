!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/29/2007
!*
!*  DESCRIPTION                : sorting algorithm used in dtpPass006
!                               sortReal8Up: sort array of real(8) ascendingly
!                               using bubble sort
!                               sortReal8Down: sort real(8) descendingly using
!                               insertion sort algorithm
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

subroutine sortReal8Up (d1, n)
    integer, intent(in) :: n
    real(8), intent(inout) :: d1(n)

    real(8) temp

    do i = n, 1, -1
        do j = 1, i
            if (d1(j) > d1(i)) then
                temp = d1(j)

                d1(j) = d1(i)
                d1(i) = temp
            end if
        end do
    end do
end subroutine


subroutine sortReal8Down (d1, n)
    integer, intent(in) :: n
    real(8), intent(inout) :: d1(n)

    real(8) temp

    do i = 2, n
        temp = d1(i)

        j = i

        do while ((j > 1) .and. (d1(j-1) < temp))
            d1(j) = d1(j-1)

            j = j - 1
        end do

        d1(j) = temp
    end do
end subroutine
