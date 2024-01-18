! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-08-22
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : test coarray dummy of integer type. Simple
!                               algotithm to add ALL elements in the coarray.
!                               Data type is default integer.
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

module sum_int_mod
    implicit none

    abstract interface
        integer function factor(x)
            integer, intent(in) :: x
        end function
    end interface

    integer, parameter :: long = selected_int_kind (18)
    contains

    ! assume no data overflow
    subroutine do_sum (x, m,n, sum_img)
        integer(long), intent(in) :: m,n, x(m,n)
        integer(long), intent(out), codimension[*] :: sum_img

        codimension x[*]

        integer :: i, j

        sum_img = 0

        do j = 1, n
            do i = 1, m
                sum_img = sum_img + x(i, j)
            end do
        end do
    end subroutine

    subroutine initialize (x, m, n, f)
        integer(long), intent(in) :: m,n
        integer(long), intent(out) :: x(m,n)
        procedure(factor) :: f

        integer :: k, i, j

        k = 1
        do j = 1, n
            do i = 1, m
                x(i, j) = f(k)
                k = k + 1
            end do
        end do
    end subroutine
end module

program sum_int02
use sum_int_mod
implicit none
    integer(long), parameter :: m = 100, n = 50
    integer(long), save, codimension[*] :: val(m,n), sum_each

    procedure (factor) f1

    call initialize(val, m, n, f1)

    call do_sum (val, m,n, sum_each)

    contains

    subroutine verify_sum
        integer :: me

        me = this_image()

        if (sum_each /= m/2*n*(m*n-1)*me) then
            print *, 'Failed on image', me
            print *, sum_each ,'vs', m/2*n*(m*n-1)*me
            error stop 1
        end if
    end subroutine
end

integer function f1 (i)
    integer, intent(in) :: i

    f1 = i * this_image()
end function
