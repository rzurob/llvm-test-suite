! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-08-11
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : test on SUM intrinsic. Simple test, use complex
!                               data type, with MASK.
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

program sumCoarray03
    implicit none
    integer, parameter :: n = 10
    complex(8), save :: x(n,n,n)[*]

    complex(8), save :: y(n,n)[*]
    logical :: mask1(n,n,n), mask2(n,n)

    complex(8), save :: sumX(n,n)[*], sumY[*]
    integer i, j, k, me
    logical, external :: precision_x6

    me = this_image()

    ! set up mask: alternative for each mask dimension
    ! set up values of x and y
    do k = 1, n
        do j = 1, n
            do i = 1, n
                mask1(i,j,k) = mod(j, 2) == 0
                x(i,j,k) = cmplx(i,k,8)*me
            end do
            mask2(j,k) = mod(j,2) == 1
            y (j,k) = cmplx(1.0d0*j, 2.0d0*k, 8)*me
        end do
    end do

    ! sum up the values of x and y
    sumX = sum(x, dim=2, mask=mask1)
    sumY = sum(y, mask=mask2)

    ! verify sumX and sumY
    do j = 1, n
        do i = 1, n
            if (.not. precision_x6 (sumX(i,j), me*n/2*cmplx(i,j,8))) then
                print *, 'Fails to verify sumX on image', me
                print *, sumX(i,j), 'vs', me*n/2*cmplx(i,j,8)
                error stop 1
            end if
        end do
    end do

    if (.not. precision_x6 (sumY, me*cmplx(250.0d0,550.d0 ,8))) then
        print *, 'Fails to verify sumY on image', me
        print *, sumY, 'vs', me*cmplx(250.0d0,550.d0 ,8)
        error stop 2
    end if

end
