! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : This test case tests the assignment of logical
!                                data type in the form of coindexed objects,
!                                also test the variable being used as LHS.
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

    logical, save :: flags(20)[0:*]
    integer, save :: i1(20)[0:*]
end module

module op_mod
    implicit none
    contains

    subroutine set_flags_on_left (m, f)
        integer, intent(in) :: m
        logical, intent(inout) :: f(m)[*]

        integer me, left, i, np

        me = this_image()
        np = num_images()

        if (me == 1) then
            left = np
        else
            left = me - 1
        end if

        f(:)[left] = [(mod(left,2) == 0, mod(left, 2) /= 0, i=1, m/2)]
    end subroutine

    subroutine set_i1_on_left
        use data_mod, only: flags, i1

        integer :: i, me, np, left

        me = this_image(flags, 1)
        np = num_images()

        if (me == 0) then
            left = np - 1
        else
            left = me - 1
        end if

        do i = 1, 20
            if (flags(i)[left]) then
                i1(i)[left] = left + 10
            else
                i1(i)[left] = -1
            end if
        end do
    end subroutine
end module

module verify_mod
    implicit none

    contains

    subroutine verify_data
        use data_mod, only: flags, i1

        integer i, me

        me = this_image()

        do i = 1, size(flags), 2
            if (mod(me, 2) == 0) then
                if ((.not. flags(i)) .or. flags(i+1)) then
                    print *, 'Error: failed to verify flags on image', me
                    print *, 'elements', i, 'and',i+1,': Expected: T F, actual:', &
                             flags(i:i+1)

                    error stop 1
                end if

                if (any(i1(2::2) /= -1)) then
                    print *, 'Failed to verify i1 on image', me
                    print *, 'expecting -1; actuals:', i1(2::2)
                    error stop 1
                end if

                if (any(i1(::2) /= 9+me)) then
                    print *, 'Failed to verify i1 on image', me
                    print *, 'expecting:', 9+me, '; actuals:',i1(::2)
                    error stop 1
                end if
            else
                if (flags(i) .or. (.not. flags(i+1))) then
                    print *, 'Error: failed to verify flags on image', me
                    print *, 'elements', i, 'and',i+1,': Expected: F T, actual:', &
                             flags(i:i+1)

                    error stop 1
                end if

                if (any(i1(::2) /= -1)) then
                    print *, 'Failed to verify i1 on image', me
                    print *, 'expecting -1; actuals:', i1(::2)
                    error stop 1
                end if

                if (any(i1(2::2) /= 9+me)) then
                    print *, 'Failed to verify i1 on image', me
                    print *, 'expecting:', 9+me, '; actuals:',i1(2::2)
                    error stop 1
                end if
            end if
        end do
    end subroutine
end module

program assign_flags
use data_mod, only : flags, i1
use op_mod, only: set_flags_on_left, set_i1_on_left
use verify_mod, only: verify_data
    implicit none

    if (num_images() < 2) then
        print *, 'Error: program requires at least 2 images to run'
        error stop 1
    end if

    call set_flags_on_left(20, flags)

    sync memory ! since we always operate on left,
                ! there is no need to call sync all

    call set_i1_on_left

    sync all

    call verify_data
end
