! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-08-11
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : test intrinsic SUM. Use co-indexed objects as
!                               Array, data type is default integer
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

program sumCoarray04
    implicit none
    integer, parameter :: n = 10, m = 15

    integer, save :: x(n,m)[*]
    integer, save :: sumX[*], sumX1[*]

    integer i, j, me, right, np, left

    me = this_image()
    np = num_images()

    if (np < 4) stop

    if (me == np) then
        right = 1
    else
        right = me + 1
    end if

    if (me == 1) then
        left = np
    else
        left = me - 1
    end if

    ! set up data for x concurrently
    call setup_x
    sync all

    ! use sum to add all the values of image right
    sumX = sum (x(:,:)[right])

    sumX1 = sum (x(:,1)[left])

    ! now validate sumX and sumX1
    call validate

    sync all

    contains

    subroutine setup_x
        integer factor

        factor = mod(me, 4)

        if (factor == 0) factor = 4

        x(:,:) = reshape([(i, i = 1, m*n)], [n,m]) * factor
    end subroutine

    subroutine validate
        integer factor_right, factor_left

        factor_right = mod(right, 4)
        factor_left = mod(left, 4)

        if (factor_right == 0) factor_right = 4
        if (factor_left == 0) factor_left = 4

        if (sumX /= factor_right * n*m*(n*m+1)/2) then
            print *, 'Fails to validate sumX on image ', me
            print *, sumX, 'vs', factor_right * n*m*(n*m+1)/2
            error stop 1
        end if

        if (sumX1 /= factor_left*n*(n+1)/2) then
            print *, 'Fails to validate sumX1 on image', me
            print *, sumX1, 'vs.', factor_left*n*(n+1)/2
            error stop 1
        end if
    end subroutine
end
