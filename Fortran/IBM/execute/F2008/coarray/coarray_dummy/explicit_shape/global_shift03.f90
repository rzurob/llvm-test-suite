! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-08-20
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : test the explicit shape coarray dummy; test a
!                                circular shift to right by 1. Change to module
!                                procedures; also use a separate coarray to
!                                store the shift result.
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

module m
    implicit none
    contains

    subroutine setup_val (x, n)
        integer, intent(in) :: n
        real(8), intent(out) :: x(n)[*]
        integer i, me

        me = this_image()

        do i = 1, n
            x(i) = i + (me-1)*n
        end do
        sync all
    end subroutine

    subroutine do_shift (x, y, n)
        integer, intent(in) :: n
        real(8), intent(inout) :: x(n)[*]
        real(8), intent(out) :: y(n)[*]
        integer :: left, np, me

        me = this_image()
        np = num_images()

        if (me == 1) then
            left = np
        else
            left = me - 1
        end if
        y(2:) = x(1:n-1)

        y(1) = x(n)[left]
        sync all
    end subroutine

    subroutine verify_val (x,y,n)
        integer, intent(in) :: n
        real(8), intent(in) :: x(n)[*], y(n)[*]

        logical, external :: precision_r8
        integer :: me, i, np, left

        me = this_image()
        np = num_images()

        if (me == 1) then
            left = np
        else
            left = me - 1
        end if

        do i = 2, n
            if (.not. precision_r8(y(i), x(i-1))) then
                print *, 'Failed to verify: i = ', i, 'on image ', me
                print *, y(i), 'vs', x(i-1)
                error stop 1
            end if
        end do

        if (.not. precision_r8(y(1), x(n)[left])) then
            print *, 'Failed to compare the 1st element on image ', me
            print *, y(1), x(n)[left]
            error stop 1
        end if
    end subroutine
end module

program global_shift02
use m
    implicit none
    integer, parameter :: n = 100
    real(8), save :: x(n)[*], y(n)[*]

    if (num_images() < 2) stop

    ! set up x values as if a whole array from 1 to np * n
    call setup_val (x, n)

    ! do the shift to right by one
    call do_shift (x, y, n)

    ! verify on image 2
    call verify_val (x,y,n)

    sync all
end

