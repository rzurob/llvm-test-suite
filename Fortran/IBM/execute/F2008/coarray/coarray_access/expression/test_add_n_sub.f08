! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-07
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : a simple test on add and subtraction of
!                               coindexed objects.
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
module arr_def
    real(8), save :: arr1(10)[*]
    real(8), save :: arr2(10)[*]
end module

module arr_op
    implicit none

    contains

    subroutine add_neighbors (x, y)
        real(8), intent(in) :: x(10)[*]
        real(8), intent(out) :: y(10)[*]

        integer me, np

        me = this_image()
        np = num_images()

        if ((me == 1) .or. (me == np)) then
            y(:) = 0.5d0*x(:)[1] + 0.5d0*x(:)[np]
        else
            y(:) = x(:)[me-1] + x(:)[me+1]
        end if
        sync all
    end subroutine

    function sub_neighbor (x) result(res)
        real(8), intent(in) :: x(10)[*]

        real(8) res(10)
        integer me, np

        me = this_image()
        np = num_images()
        if (me == 1) then
            res = x(10:1:-1)[2] - x(:)[1]
        else if (me == np) then
            res = x(10:1:-1)[me] - x(:)[me-1]
        else
            res = x(:)[me+1] - x(10:1:-1)[me-1]
        end if
    end function
end module

program test_add_n_sub
use arr_def
use arr_op
    implicit none
    integer i, me, np
    logical, external :: precision_r8

    me = this_image()
    np = num_images()

    arr1 = [(i, i = (me-1)*10+1, 10*me)]

    sync all

    call add_neighbors (arr1, arr2)

    if ((me == 1) .or. (me == np)) then
        do i = 1, 10
            if (.not. precision_r8(arr2(i), (i+(np-1)*10+i)/2.0d0)) then
                print *, 'validation failed for image ', me
                print *, arr2(i), 'vs', (i+(np-1)*10+i)/2.0d0
                error stop 1
            end if
        end do
    else
        do i = 1, 10
            if (.not. precision_r8(arr2(i), (20*me + 2*i- 20)*1.0d0)) then
                print *, 'validation failed for image', me
                print *, arr2(i), 'vs', (20*me + 2*i- 20)*1.0d0
                error stop 2
            end if
        end do
    end if

    arr2(:)[me] = sub_neighbor (arr1)

    call verify_all (arr2)

    contains

    subroutine verify_all (y)
        real(8), intent(in) :: y(:)

        integer i

        if ((me == 1) .or. (me == np)) then
            do i = 1, 10
                if (.not. precision_r8(y(i), 21.0d0 - 2.0d0*i)) then
                    print *, 'Error: validation on image', me
                    error stop 1
                end if
            end do
        else
            do i = 1, 10
                if (.not. precision_r8(y(i), 9.0d0 + 2.d0*i)) then
                    print *, 'Error: validation on image', me
                    error stop 1
                end if
            end do
        end if
    end subroutine
end
