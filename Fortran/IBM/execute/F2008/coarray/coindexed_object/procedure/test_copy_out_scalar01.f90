! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-20
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : simple test that coindexed objects are copied
!                               out when function call finishes with
!                               intent(inout) or intent(out) dummy.
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
    integer, save :: i4[*]
    integer(8), save :: i8[*]
    integer(2), save :: i2[*]
    integer(1), save :: i1[*]
end module

module op_mod
    implicit none
    interface set
        procedure set_int1
        procedure set_int2
        procedure set_int4
        procedure set_int8
    end interface

    contains

    subroutine set_int1 (i, val)
        integer(1), intent(inout) :: i
        integer(1), intent(in) :: val

        i = val
    end subroutine

    subroutine set_int2 (i, val)
        integer(2), intent(out) :: i
        integer(2), intent(in) :: val

        i = val
    end subroutine

    subroutine set_int4 (i, val)
        integer, intent(out) :: i
        integer, value :: val

        i = val
    end subroutine

    subroutine set_int8 (i, val)
        integer(8), intent(inout) :: i
        integer(8), intent(in) :: val

        i = i + val
    end subroutine
end module

use data_mod
use op_mod, only: set
    implicit none

    integer me, np, left, right

    me = this_image()
    np = num_images()

    if (me == 1) then
        left = np
    else
        left = me - 1
    end if

    if (me == np) then
        right = 1
    else
        right = me + 1
    end if

    i8 = me
    sync all

    ! test integers of kind 1, 2, 4 and 8
    call set (i1[right], int(right+1, 1))
    call set (i2[left], int(left-1, 2))
    call set (i4[me], me*2)
    call set (i8[right], right*2_8)

    sync all
    if (np < 128) then
        if (i1 /= (me + 1)) then
            print *, 'Error: i1 validation failed on', me
            print *, i1, 'vs', me+1
            error stop 1
        end if
    end if

    if (np < 2**15) then
        if (i2 /= (me - 1)) then
            print *, 'Error: i2 validation failed on', me
            print *, i2, 'vs', me - 1
            error stop 1
        end if
    end if

    if (i4 /= 2*me) then
        print *, 'Error: i4 validation failed on', me
        print *, i4, 'vs', me*2
        error stop 1
    end if

    if (i8 /= 3*me) then
        print *, 'Error: i8 validation failed on', me
        print *, i8, 'vs', me*3
        error stop 1
    end if
end
