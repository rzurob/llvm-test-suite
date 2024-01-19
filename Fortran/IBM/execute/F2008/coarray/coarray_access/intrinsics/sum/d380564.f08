! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-01-11
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : defect 380564: array section call in coindexed
!                               object reference.
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
    implicit none
    real, save :: x(2)[*]
    real sumRight
    integer me, i, np, right

    logical, external :: precision_r4

    me = this_image()
    np = num_images()

    if (np < 2) then
        print *, 'Error: program requires at least 2 images to run'
        error stop 1
    end if

    if (me == np) then
        right = 1
    else
        right = me + 1
    end if

    !! set the values of x on all images from image 1
    if (me == 1) then
        do i = 1, np
            x(:)[i] = [i, i*10]
        end do
    end if
    sync all
    sumRight = sum(x(:)[right])

    if (.not. precision_r4(sumRight, right*11.0)) then
        print *, 'failed to verify sumRight on image', me
        print *, 'Expected:',right*11.0,'. Actual:',sumRight
        error stop 1
    end if

    if (.not. precision_r4(sum(x(:)[right]), right*11.0)) then
        print *, 'failed to verify sum(:)[right] on image', me
        print *, 'Expected:',right*11.0,'. Actual:',sum(x(:)[right])
        error stop 1
    end if
end
