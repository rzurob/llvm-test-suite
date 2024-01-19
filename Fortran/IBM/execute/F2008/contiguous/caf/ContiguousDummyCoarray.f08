! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-08-22
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
!*  11/29/10   D.B.   The original test case was modified to test CONTIGUOUS
!*                    attribute with assumed shape dummy argument
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module sum_int_mod
    implicit none

    abstract interface
        integer function factor(x)
            integer, intent(in) :: x
        end function
    end interface

    contains

    ! assume no data overflow
    subroutine do_sum (x, m,n, sum_img)
        integer, intent(in) :: m,n
        integer, intent(in), contiguous :: x(:,:)
        integer, intent(out), codimension[*] :: sum_img

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
        integer, intent(in) :: m,n
        integer, intent(out) :: x(m,n)
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

program sum_int01
use sum_int_mod
implicit none
    integer, parameter :: m = 100, n = 50
    integer, save, codimension[*] :: val(m,n), sum_each

    procedure (factor) f1

    call initialize(val, m, n, f1)

    call verify_init

    contains

    subroutine verify_init
        integer :: i, j, k, me

        me = this_image()

        k = 1
        do j = 1, n
            do i = 1, m
                if (val(i,j) /= me *k) then
                    print *, 'Failed to verify value at (', i, ',',j,') on image', me
                    print *, val(i,j), 'vs', me * k
                    error stop 1
                end if
                k = k + 1
            end do
        end do
    end subroutine
end

integer function f1 (i)
    integer, intent(in) :: i

    f1 = i * this_image()
end function
