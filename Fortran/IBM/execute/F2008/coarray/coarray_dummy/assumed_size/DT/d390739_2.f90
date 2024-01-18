! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-07-25
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : This is an alternative to defect 390739: use
!                               of a declared array instead of temps created by
!                               array constructor.
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
    integer :: me, np, left, right
    integer neighbors(2)

    np = num_images()
    me = this_image()

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

    neighbors = [left, right]

    print *, me, ': sync images with', left, right

    sync images(neighbors)

    print *, me, 'all done'
    end
