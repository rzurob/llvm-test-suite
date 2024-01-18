! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-27
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : a simple test on average file routine.
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
program field_avg
use distribute_module, only: double, distribute_data
use compute_average_mod, only: average_field

    implicit none
    character(20), parameter :: filename = 'input.dat'
    integer, parameter :: local_size = 1000
    integer :: resolution

    integer me, np, img, i, idx
    real(double), save :: x(local_size)[*], y(local_size)[*]
    real(double) temp

    me = this_image()
    np = num_images()

    if (np < 2) then
        print *, 'needs at least two images to run'
        error stop 1
    end if

    resolution = np * local_size

    if (this_image() == 1) then
        call createField (filename, resolution)
    end if

    sync all

    ! distribute data is done on image 1
    call distribute_data (filename,local_size, x)

    ! compute average is done on all images
    call average_field (x, y, local_size)

    !! produce files to compare
    if (me == np) then
        open (2, file='average.out')

        do img = 1, np
            do i = 1, local_size
                write (2, *) i+local_size*(img-1),y(i)[img]
            end do
        end do

        close(2)
    else if (me == 1) then
        call create_avg(filename, 'average.vf')
    end if

end

!this routine reads in the data from input.data and do an average on three
!consective points and write out the average field in the output
subroutine create_avg (infile, outfile)
    use distribute_module, only: double
    implicit none
    character(*), intent(in) :: infile, outfile
    integer iarr(3), i, istat, idx
    real(double) darr(3), d

    open (1, file=infile, status='old')
    open (10, file=outfile, status='new')

    idx = 1
    do
        read(1, *, iostat=istat) i, d
        if (istat /= 0) then
            write (10, *) iarr(3), darr(3)
            exit
        end if

        if (idx == 1) write (10, *) i, d

        iarr(idx) = i
        darr(idx) = d

        if (idx == 3) then
            write (10, *) iarr(2), sum(darr(:))/3.0d0

            iarr(1:2) = iarr(2:3)
            darr(1:2) = darr(2:3)
        else
            idx = idx + 1
        end if
    end do

    close(10)
    close(1)
end subroutine

! this routine will calculate a sine field (0, 2*pi)
! in a file; n is the number of points to track the field
subroutine createField (filename, n)
    use distribute_module, only: double
    implicit none
    character(*), intent(in) :: filename
    integer, intent(in) :: n

    real(double), parameter :: pi = 4.0d0*atan(1.0d0)
    integer i
    real(double) dx

    dx = 2*pi/n

    open (1, file = filename, status = 'new')

    do i = 1, n
        write (1, *) i, dsin(dx*i)
    end do

    close (1)
end subroutine
