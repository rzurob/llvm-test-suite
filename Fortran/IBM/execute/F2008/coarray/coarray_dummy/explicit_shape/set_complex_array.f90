! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-08-29
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : test coarray dummy. Test complex data type,
!                               lower cobounds being dummy integer.
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

module test
    implicit none
    contains

    ! this routine set the value of x(me)[co_n] as complex(me, me)
    ! this means that only the array X on image 1 is set.
    subroutine set_array (x, n,co_n)
        complex, intent(out) :: x(n)[co_n:*]
        integer, intent(in) :: n, co_n

        integer :: me, np

        me = this_image()
        np = num_images()

        if (n <  np) then
            print *, 'Input data invalid: n (', n, 'must be at least',np
            error stop 1
        end if

        x(me)[co_n] = cmplx(me, me)  ! there is no race here
        sync all
    end subroutine
end module

program set_complex_array
use test
    integer, parameter :: n = 10000
    complex, save :: coarr(n)[*]
    logical, external :: precision_x8

    integer :: me, np, i

    me = this_image()
    np = num_images()

    call set_array (coarr, n, me)

    ! verify the results
    if (me == 1) then
        do i = 1, np
            if (.not. precision_x8 (coarr(i), cmplx(i,i))) then
                print *, 'verification fails'
                print *, coarr(i), 'vs', cmplx(i,i)
                error stop 1
            end if
        end do
    end if
end
