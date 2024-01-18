! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-14
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : a simple test on use of coindexed objects in
!                               power (**).
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
    integer, parameter :: arr_size = 10
    complex(8), save :: cx(arr_size)[0:*]
    integer, save :: coeff[0:*]

    contains

    subroutine initialize
        integer me, np, i

        me = this_image()
        np = num_images()

        if (mod(me, 2) == 0) then
            coeff = 2

            do i = 1, arr_size
                cx(i) = cmplx(i, me, 8)
            end do
        else
            cx(:) = cmplx(me, [(i, i = 1, arr_size)], 8)
            coeff = 3
        end if
        sync all
    end subroutine

    subroutine compute
        integer me, neighbor

        me = this_image()

        if (me == 1) then
            neighbor = 1
        else
            neighbor = me - 2
        end if

        cx = cx**coeff[neighbor]
        sync all
    end subroutine
end module

use m
    implicit none
    integer me, np, i
    logical, external :: precision_x6

    me = this_image()
    np = num_images()

    if (np < 2) then
        stop 'program needs at least two images'
    end if

    call initialize

    call compute
    ! verify the computational results
    do i = 1, arr_size
        if (mod(me, 2) == 0) then
            if (.not. precision_x6(cx(i), cmplx(i, me,8)**3)) then
                print *, 'verification failed on', me, 'for i=', i
                print *, cx(i), 'vs', cmplx(i, me,8)**3
                error stop 1
            end if
        else
            if (.not. precision_x6(cx(i), cmplx(me,i, 8)**2)) then
                print *, 'verification failed on', me, 'for i=', i
                print *, cx(i), 'vs', cmplx(me,i,8)**2
                error stop 1
            end if
        end if
    end do
end
