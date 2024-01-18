! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-02
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : defect 380692
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
            integer, codimension[*], intent(in) :: x(3)
        end function
    end interface

    integer, codimension[*], save :: coarr(10)
    integer i, np
    integer(long) sumVal, sumExpected

    np = num_images()

    ! sets up data for testing
    coarr(:) = [(i, i = 1, 10)]*this_image()
    sync all

    sumVal = sum3(coarr)
    sumExpected = 3_long * np * (np+1)
    if (sumVal /= sumExpected) then
        print *, 'verification failed on image', this_image()
        print *, sumVal,'vs',  sumExpected
        error stop 1
    end if
end

    function sum3 (x)
        use params
        implicit none
        integer, codimension[*], intent(in) :: x(3)
        integer(long) sum3

        integer np, i

        np = num_images()

        sum3 = 0

        do i = 1, np
            sum3 = sum3 + x(1)[i] + x(2)[i] + x(3)[i]
        end do
    end function
