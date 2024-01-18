! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-11-22 (original: 2010-09-02)
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : adaptation of F2008/coarray/coarray_dummy/explicit_shape/d380692
!*                               - coarray scalar used in place of coarray array
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

module params
    integer, parameter :: long = selected_int_kind (18)
end module

program d380692
    use params
    implicit none
    interface
        integer(long) function sum3 (x)
            import
            integer, codimension[*], intent(in) :: x
        end function
    end interface

    integer, codimension[*], save :: coarr
    integer i, np
    integer(long) sumVal, sumExpected

    np = num_images()

    ! sets up data for testing
    coarr = this_image()
    sync all

    sumVal = sum3(coarr)
    sumExpected = np * (np+1) / 2
    if (sumVal /= sumExpected) then
        print *, 'verification failed on image', this_image()
        print *, sumVal,'vs',  sumExpected
        error stop 1
    end if
end

    function sum3 (x)
        use params
        implicit none
        integer, codimension[*], intent(in) :: x
        integer(long) sum3

        integer np, i

        np = num_images()

        sum3 = 0

        do i = 1, np
           sum3 = sum3 + x[i]
        end do
    end function
