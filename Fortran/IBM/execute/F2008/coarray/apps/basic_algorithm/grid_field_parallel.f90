! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : This is a basic test on parallel field setup by
!                                all images.  The verification is done on one
!                                image.
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

    program grid_field_setup
    implicit none

    integer, parameter :: r1_size = 2000
    double precision, parameter :: PI = 4.0d0*atan(1.0d0)
    double precision, save :: x(r1_size)[*]
    integer :: np, total_grid, i,j, k
    double precision, allocatable :: x_verify(:)

    np = num_images()

    total_grid = np * r1_size

    x = gen_grid(r1_size)

    sync all

    if (this_image() == 1) then
        allocate (x_verify(np*r1_size))

        k = 1
        do i = 1, np
            do j = 1, r1_size
                x_verify(k) = x(j)[i]
                k = k + 1
            end do
        end do

        !! now verify the result
        do i = 1, np*r1_size
            if (.not. my_precision_r8(x_verify(i), sin(2*i*PI/(np*r1_size)), &
                                      1.0d-11)) then
                print *, i, x_verify(i), sin(2*i*PI/(np*r1_size))
                error stop 1_4
            end if
        end do
    end if

    sync all  !<-- required since END PROGRAM statement not yet implemented

    contains

    function gen_grid(local_grid_size)
        integer, intent(in) :: local_grid_size

        double precision gen_grid (local_grid_size)

        double precision grid_interval
        integer i

        grid_interval = 2*PI/(local_grid_size*np)

        do i = 1, local_grid_size
            gen_grid(i) = sin(((this_image()-1)*local_grid_size+i)*grid_interval)
        end do
    end function

    logical function my_precision_r8 (d1, d2, limit)
        real(8), intent(in) :: d1, d2, limit

        my_precision_r8 = abs(d1-d2) <= abs(d1+d2)*limit/2.0d0
    end function
    end
