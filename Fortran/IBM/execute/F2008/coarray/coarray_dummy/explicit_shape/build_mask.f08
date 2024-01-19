! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-08-23
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : This test case build a mask coarray field. User
!                               chooses the first mask element.  The mask
!                               pattern is T T F F (viewed globally).
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

    contains

    subroutine make_mask (mask, n, ibegin, img)
        logical, intent(out), codimension[*], dimension(n) :: mask
        integer, intent(in) :: n, ibegin, img

        integer :: np, i,j, k
        logical, parameter :: pattern(4) = [.true., .true., .false., .false.]

        np = num_images()

        if ((img < 1) .or. (img > np)) then
            print *, img, 'is an invalid image index. Range: 1 --', np
            error stop 1
        end if

        if (ibegin > n) then
            print *, ibegin, 'is an invalid index. Range: 1 -- ', n
            error stop 1
        end if

        ! we do the mask building on image 1. This is sequential code
        if (this_image() == 1) then
            k = 1

            do j = ibegin, n
                mask(j)[img] = pattern(k)
                call increase(k)
            end do

            do i = img+1, np
                do j = 1, n
                    mask(j)[i] = pattern(k)
                    call increase(k)
                end do
            end do

            do i = 1, img - 1
                do j = 1, n
                    mask(j)[i] = pattern(k)
                    call increase(k)
                end do
            end do

            do j = 1, ibegin - 1
                mask(j)[img] = pattern(k)
                call increase(k)
            end do
        end if
        sync all

        contains

        subroutine increase (i)
            integer, intent(inout) :: i
            if (i == 4) then
                i = 1
            else
                i = i + 1
            end if
        end subroutine
    end subroutine
end module

program build_mask
use m
    implicit none
    integer, parameter :: nsize = 20
    logical, save, codimension[*], dimension(nsize, nsize) :: flags
    logical, parameter :: verify_pattern(4) = [.true., .true., .false., .false.]

    integer :: i,j, np

    np = num_images()

    if (np < 2) stop 'sequential program'

    ! set up mask, head at image 2, element 41
    call make_mask (flags, nsize*nsize, 41, 2)  !<-- this mask is TTFFTTFF...

    ! verify on all images
    if (any([flags] .neqv. [(verify_pattern, i = 1, 100)])) then
        print *, 'Failed verification on image ', this_image()
        print *, 'Expecting ', [(verify_pattern, i = 1, 100)]
        print *, flags
        error stop 1
    end if

    sync all
    ! next test: set up mask, head at image np-1, element 11
    call make_mask (flags, nsize*nsize, 11, np - 1) !<-- the mask is FFTTFFTT...

    ! verify on all images
    if (any([flags] .neqv. [(verify_pattern(3:4), verify_pattern(1:2), i = 1, 100)])) then
        print *, 'Failed verification on image ', this_image()
        print *, 'Expecting ', [(verify_pattern, i = 1, 100)]
        print *, flags
        error stop 1
    end if
end program
