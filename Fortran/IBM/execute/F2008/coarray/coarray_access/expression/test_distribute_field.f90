! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-26
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : a simple test on data distribution
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

program test_distribute_field
use distribute_module, only : distribute_data, double
    implicit none

    integer, parameter :: sz = 100

    real(double), save :: x(sz)[*]
    integer me, np

    me = this_image()
    np = num_images()

    call write_field ("Cosine.dat", np*sz)

    call distribute_data("Cosine.dat", sz, x)

    ! now verify the results of data distribution
    call verify_data

    contains

    subroutine verify_data
        integer i
        logical, external :: precision_r8
        real(double), parameter :: pi = 4.0d0*atan(1.0d0)
        real(double) dx

        dx = 2.0d0*pi/(np*sz)

        do i = 1, sz
            if (.not. precision_r8 (x(i), dcos((i+(me-1)*sz)*dx))) then
                print *, 'verify data failed on image', me, 'for element', i
                print *, x(i), 'vs', dcos((i+(me-1)*sz)*dx)
                error stop 1
            end if
        end do
    end subroutine
end

subroutine write_field (filenm, resolution)
    use distribute_module, only : double
    implicit none
    integer, intent(in) :: resolution
    character(*), intent(in) :: filenm

    real(double), parameter :: pi = 4.0d0*atan(1.0d0)

    real(double) dx
    integer i

    dx = 2*pi/resolution

    if (this_image() == num_images()) then
        open (1, file=filenm, status='new')
        write (1, *) (i, dcos(dx*i), new_line('a'), i = 1, resolution)
        close(1)
    end if

    sync all
end subroutine

