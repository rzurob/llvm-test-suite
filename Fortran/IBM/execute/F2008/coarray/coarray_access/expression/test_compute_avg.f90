! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-09-26
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : a simple test on compute_avg
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
program test_compute_avg
    use compute_average_mod, only:average_field
    use distribute_module, only: double
    implicit none

    real(double), save :: x(10)[*], y(10)[*]
    real(double), allocatable :: local(:), local_y(:)
    integer i, j, k, np
    logical, external :: precision_r8

    if (num_images() < 2) then
        stop 10
    end if

    x = [(log(i + (this_image() - 1)*10.0d0), i = 1, 10)]

    sync all

    call average_field (x, y, 10)

    np = num_images()
    ! verify the results
    if (this_image() == num_images()) then
        local = [(log(i*1.0d0), i = 1, 10*np)]

        allocate (local_y(10*np))

        local_y(1) = local(1)

        local_y(10*np) = local(10*np)

        do i = 2, 10*np - 1
            local_y(i) = (local(i-1) + local(i) + local(i+1))/3.0d0
        end do

        k = 1
        do j = 1, np
            do i = 1, 10
                if (.not. precision_r8(local_y(k), y(i)[j])) then
                    print *, 'verify data failed on image', j, 'for element', i
                    print *, y(i)[j], 'vs', local_y(k)
                    error stop 1
                end if
                k = k + 1
            end do
        end do
    end if
end
