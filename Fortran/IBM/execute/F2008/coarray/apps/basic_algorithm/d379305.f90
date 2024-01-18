! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-09-07
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : defect 379305
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
    integer, parameter :: N = 1024**2
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

    contains

    subroutine verify_data
        integer j, factor
        logical, external :: precision_r8

        factor = this_image()

        do j = 1, N
            if (.not. precision_r8(field(j), factor*dlog(j*1.0d0))) then
                print *, 'image', this_image(), ', expecting:', &
                factor*dlog(j*1.0d0), 'actual:', field(j)

                error stop 1_4
            end if
        end do
    end subroutine
end program

