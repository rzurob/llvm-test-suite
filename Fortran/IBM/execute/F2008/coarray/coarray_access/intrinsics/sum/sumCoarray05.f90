! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-08-12
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : test intrinsic SUM on coarrays. Data type is
!                               default real, SUM on both coarrays and
!                               co-indexed objects.
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

program sumCoarray05
    implicit none
    integer, parameter :: n = 100
    real, save :: sumVal(n)[*], x(n,n)[*]
    integer :: i, j, me, np, k
    logical, external :: precision_r4
    real sumAll

    me = this_image()
    np = num_images()

    ! this test intends to be run with at least 2 images
    if (np < 2) stop

    ! set up value for x
    if (me == 2) then
        do k = 1, np
            do j = 1, n
                do i = 1, n
                    x(i,j)[k] = i * 1.25 *k
                end do
            end do
        end do
    end if
    sync all

    sumVal = sum (x, dim = 1, mask=.true.)

    ! verify sumX on all images
    do i = 1, n
        if (.not. precision_r4(sumVal(i), me*n*(n+1)*.5*1.25)) then
            print *, 'validation of sumX(i) fails for i =', i, ', on image', me
            print *, sumVal(i), 'vs', me*n*(n+1)*.5*1.25
            error stop 1
        end if
    end do

    sync all
    !now add all the sumX by one image to see if the values add up
    if (me == 1) then
        sumAll = 0

        do i = 1, np
            sumAll = sumAll + sum (sumVal(:)[i])
        end do

        if (.not. precision_r4(sumAll, 0.5*np*(np+1)*n*n*(n+1)*0.5*1.25)) then
            print *, 'validation of sumAll on image 1 fails'
            print *, sumAll, 'vs', 0.5*np*(np+1)*n*n*(n+1)*0.5*1.25
            error stop 1
        end if
    end if
end
