! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-10-07
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : a simple test on multiplication on coindexed
!                               objects.
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

module var_mod
    implicit none
    integer(8), save :: arr1(10)[*], arr2(10)[*], arr3(10)[*]

    integer(8) local(10)

    contains

    subroutine initial_val (x)
        integer(8), intent(inout), codimension[*] :: x(10)

        integer me, np, i

        me = this_image()
        np = num_images()

        x = [(i+10*me-10, i = 1, 10)]

        sync all
    end subroutine
end module

module multiply_mod
    implicit none

    contains

    function multiply (n, a, b) result(res)
        integer, intent(in) :: n
        integer(8), intent(in), codimension[*] :: a(n), b(n)

        integer(8), dimension(n) :: res

        integer me, np

        me = this_image()
        np = num_images()

        if ((me == 1) .or. (me == np)) then
            res = (a+b)/2
        else
            res = a(:)[me-1] * (b(:)[me + 1] - 2*n)
        end if
    end function
end module


subroutine verify_data (x)
    implicit none
    integer(8), intent(in) :: x(10)

    integer me, np, i

    me = this_image()
    np = num_images()

    if ((me == 1) .or. (me == np)) then
        if (any(x /= [(i+10*me-10, i = 1, 10)])) then
            print *, 'validation failed on image', me
            print *, x
            print *, [(i+10*me-10, i = 1, 10)]
            error stop 1
        end if
    else
        do i = 1, 10
            if (x(i) /= (me*10 -20 +i)**2) then
                print *, 'failed to validate on image', me
                print *, 'for element', i
                print *, x(i), 'vs', (me*10 -20 +i)**2
                error stop 1
            end if
        end do
    end if
end subroutine

use var_mod
use multiply_mod, only : multiply
    external verify_data

    call initial_val(arr1)
    call initial_val(arr2)

    local = multiply(size(arr1), arr1, arr2)
    arr3 = multiply (size(arr2), arr2, arr1)

    call verify_data(local)
    call verify_data(arr3)
end
