! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/06/2010
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : test coarray data distribution: simulates
!                                MPI_bcast call.
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

program field_broadcast
    implicit none
    integer, parameter :: N = 128**2
    real(8), save :: field(N)[*]
    integer :: i

    if (this_image() == 1) then
        do i = 1, N
            field(i) = dlog(i*1.0d0)
        end do
    end if
    sync all

    ! all images get the field value
    if (this_image() /= 1) &
        field(:) = field(:)[1]*this_image()

    ! now verify
    call verify_data

    sync all  !<- needed since END PROGRAM is not implemented yet
    contains

    logical function my_precision_r8 (x, y, relative)
        real(8), intent(in) :: x, y, relative

        my_precision_r8 = abs(x-y) <= abs(x+y)*relative*0.5d0
    end function

    subroutine verify_data
        integer j, factor

        factor = this_image()

        do j = 1, N
            if (.not. my_precision_r8(field(j), factor*dlog(j*1.0d0), 1.0d-12)) then
                print *, 'image', this_image(), ', expecting:', &
                factor*dlog(j*1.0d0), 'actual:', field(j)+0.0d0

                error stop 1_4
            end if
        end do
        print *, 'image', this_image()
    end subroutine
end program
