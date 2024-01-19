! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-08-23
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : test that an array section can be used as the
!                               actual argument to be associated with a dummy.
!                               The testable statement is a suobject of coarray
!                               is a coarray if it doesn't include cosubscripts,
!                               vector subscripts, allocatable component
!                               selection or pointer component selection.
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

    integer function sum3 (x, img)
        integer, codimension[*], intent(in) :: x(3)
        integer, intent(in) :: img

        if ((img < 1) .or. (img > num_images())) then
            print *, 'input image index', img, ' is invalid'
            print *, 'range: from 1 to', num_images()
            error stop 1
        end if

        sum3 = x(1)[img] + x(2)[img] + x(3)[img]
    end function
end module

program sum_int03
use m
    implicit none
    integer, save, codimension[*] :: arr(10)
    integer :: sum_val, me, np

    np = num_images()

    if (np < 2) stop 'sequential program'

    me = this_image()

    call set_up_arr
    sync all

    sum_val = sum3(arr, 2) !<-- pass whole array

    if (sum_val /= 12) then
        print *, 'verification failed on image', me
        print *, 'expecting 12, getting', sum_val
        error stop 1
    end if

    if (me == 1) then
        sum_val = sum3(arr(3:), np) !<-- pass a section

        if (sum_val /= 12*np) then
            print *, 'sum3(arr(3:), np) failed on image 1'
            print *, sum_val , 'vs', np
            error stop 1
        end if
    end if

    sum_val = sum3(arr(5), 1)  !<-- pass an element
    if (sum_val /= 18) then
        print *, 'verification failed on image', me
        print *, 'expecting 18, getting', sum_val
        error stop 1
    end if

    contains

    ! this routine sets up the values for arr
    subroutine set_up_arr
        integer i
        do i = 1, size(arr)
            arr(i) = i * this_image()
        end do
    end subroutine
end
