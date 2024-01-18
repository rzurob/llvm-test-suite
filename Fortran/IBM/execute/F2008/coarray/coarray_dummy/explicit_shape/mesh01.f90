! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-02
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Treat the coarray of two codimensions as a
!                               mesh. Use scalars now. Test case requires 16
!                               images to run.
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

program mesh01
    implicit none
    complex, save :: d1 [*], d2[4,*]
    integer, save :: vd1[4,*], vd2[2,*]
    logical, external :: precision_x8

    integer :: i, j, me, np

    np = num_images()
    me = this_image()

    if (np /= 16) then
        print *, 'requires 16 images to run.'
        stop 10
    end if

    call set_up (d1, 4)
    call set_up (d2, 2)

    sync all

    ! now the verification part
    do i = 1, 4
        do j = 1, 4
            if(image_index(vd1,[i,j]) == me) then
                if (.not. precision_x8(d1, cmplx(i,j))) then
                    print *, 'verification of d1 failed on image', me
                    print *, d1, 'vs', cmplx(i,j)
                    error stop 1
                end if
            end if
        end do
    end do

    do i = 1, 2
        do j = 1, 8
            if (image_index(vd2,[i,j]) == me) then
                if (.not. precision_x8(d2, cmplx(i,j))) then
                    print *, 'verification of d2 failed on image', me
                    print *, d2, 'vs', cmplx(i,j)
                    error stop 1
                end if
            end if
        end do
    end do

    contains

    subroutine set_up (x, m)
        integer, intent(in) :: m
        complex, intent(out) :: x[m,*]
        integer i(2)

        i = this_image(x)

        x = cmplx (i(1), i(2))
    end subroutine
end
