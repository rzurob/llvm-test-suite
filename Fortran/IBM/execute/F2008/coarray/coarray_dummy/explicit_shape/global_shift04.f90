! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-09-16
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : global shift to right by one.  Test nested
!                               function calls with coarray dummy.
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

    subroutine do_shift (n, co1, co2)
        integer, intent(in) :: n
        real, intent(in) :: co1(n)[n, *]
        real, intent(inout) :: co2(n)[n, *]

        sync all !<-- in case there is no sync yet
        co2 = get_left(n, co1)
        sync all
    end subroutine

    function get_left(n, x) result(res)
        integer, intent(in) :: n
        real, intent(in) :: x(n)[*]
        real res(n)

        integer :: me, np, left

        me = this_image()
        np = num_images()

        if (me == 1) then
            left = np
        else
            left = me - 1
        end if

        res = x(:)[left]
    end function
end module

program global_shift04
use m, only : get_left, do_shift
    implicit none
    integer, parameter :: n_size = 100

    real, save :: co1(n_size)[n_size,*], co2(n_size)[n_size,*]

    integer :: me, np, i

    me = this_image()
    np = num_images()

    if (np < 2) error stop "test requires at least two images"

    co1 = log([(i*1.1*me, i = 1, n_size)])
    co2 = -1.0

    call do_shift(n_size, co1, co2)

    call verify(n_size, co2)

    contains

    subroutine verify(n,x)
        integer, intent(in) :: n
        real, intent(in) :: x(n)

        integer i, left
        logical, external :: precision_r4

        if (me == 1) then
            left = np
        else
            left = me - 1
        end if

        do i = 1, n
            if (.not. precision_r4(x(i), log(i*1.1*left))) then
                print *, 'Error in verify the element', i, 'on image', me
                print *, x(i), 'vs', log(i*1.1*left)
                error stop 1
            end if
        end do
    end subroutine
end

