! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-08-19
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : test the explicit shape coarray dummy; test a
!                                circular shift to right by 1.
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

program global_shift
    implicit none
    integer, parameter :: n = 100
    real, save :: x(n)[*]
    integer :: me

    me = this_image()
    if (num_images() < 2) stop

    ! set up x values as if a whole array from 1 to np * n
    call setup_val (x)
    sync all

    ! do the shift to right by one
    call do_shift (x)
    sync all

    ! verify on image 2
    call verify_val (x)

    sync all
    contains

    subroutine setup_val (x)
        real, intent(out) :: x(n)[*]
        integer i

        do i = 1, n
            x(i) = i + (me-1)*n
        end do
    end subroutine

    subroutine do_shift (x)
        real, intent(inout) :: x(n)[*]
        real temp
        integer :: right, np

        np = num_images()

        if (me == np) then
            right = 1
        else
            right = me + 1
        end if
        temp = x(n)

        x(2:) = x(1:n-1)

        sync all   !<-- this is a big hammer, should be replaced by sync image
                   !     (left) later
        x(1)[right] = temp
    end subroutine

    subroutine verify_val (x)
        real, intent(in) :: x(n)[*]

        integer :: i,j, np
        logical, external :: precision_r4
        real temp(n)
        real, allocatable :: verify_data(:)


        if (me == 2) then
            np = num_images()
            allocate (verify_data(n*np))

            verify_data = cshift([(i*1.0, i=1,np*n)], -1)

            do i = 1, np
                temp = x(:)[i]

                do j = 1, n
                    if (.not. precision_r4(temp(j), verify_data((i-1)*n + j))) then
                        print *, 'fails to verify x(', j,') on image', i
                        print *, temp(j), verify_data((i-1)*n + j)
                        error stop 1
                    end if
                end do
            end do
        end if
    end subroutine
end
