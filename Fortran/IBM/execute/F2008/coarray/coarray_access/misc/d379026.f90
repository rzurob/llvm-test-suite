! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-15
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : defect 379026.
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

program d379026
    implicit none
    real, save :: cox(5)[*]
    real, allocatable :: x(:)
    integer np, i, j, k
    logical, external :: my_precision_r4

    np = num_images()

    if (this_image() == 1) then
        x = log([(k*1.2, k = 1, 5*np)])

        k = 1
        do i = 1, np
            do j = 1, 5
                cox(j)[i] = x(k)
                k = k + 1
            end do
        end do
    end if

    if (allocated(x)) deallocate (x)
    sync all

    ! verify at the last image
    if (this_image() == np) then
        k = 1

        do i = 1, np
            do j = 1, 5
                if (.not. my_precision_r4(cox(j)[i], log(k*1.2))) then
                    print *, 'failed to verify element', j, 'on image', i
                    print *, cox(j)[i], 'vs', log(k*1.2)
                    error stop 1
                end if
                k = k + 1
            end do
        end do
    end if
    stop
end

logical function my_precision_r4(a, b)
    implicit none
    real, intent(in) :: a, b

    my_precision_r4 = abs(a-b) <= abs(a+b)*5.0e-6
end function
