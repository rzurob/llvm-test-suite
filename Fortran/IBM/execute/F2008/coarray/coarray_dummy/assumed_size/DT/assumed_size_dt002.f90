! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-07-13
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : a simple test on assumed size coarray dummy
!*                              The algorithm is to reverse an array component
!                               of the dummy and also do a sum operation on two
!                               images. The difference from assumed_size_dt001
!                               is that coindexed objects access is made.
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
module m
    implicit none

    character*(*), parameter :: REVERSE = 'reverse'

    type val_array
        character(10) :: flag = ''
        double precision :: sum = 0.0
        integer :: values(100) = 0
    end type

    contains

    !this routine does reversal on elemnet order for
    ! each a(1:n) if the flag == 'reverse'
    subroutine reverse_order_n_sum (a, n)
        integer, intent(in) :: n
        type(val_array), codimension[*], dimension(*) :: a

        integer i, j, me
        double precision tmp

        me = this_image()

        do i = 1, n
            ! if the values are to be reversed; then do the reversal
            if (a(i)%flag == 'reverse') then
                a(i)%sum = 0.0
                !swap by pairs: values(j) .swap. values(101-j)
                do j = 1, 100, 2
                    tmp = a(i)%values(j)
                    a(i)%values(j) = a(i)%values(101-j)
                    a(i)%values(101-j) = tmp

                    a(i)%sum = a(i)%sum + a(i)%values(j) + a(i)%values(101-j) &
                             + a(i)[me+1]%values(j) + a(i)[me+1]%values(101-j)
                end do
            end if
        end do
    end subroutine
end module

module m1
    use m, only : val_array, reverse
    implicit none

    type (val_array), save :: x(10)[*]

    contains

    subroutine initialize_x ()
        integer i, j, me, np, neighbor

        me = this_image()
        np = num_images()

        if (mod(np, 2) /= 0) then
            stop 10
        end if

        ! set up my neighbor
        if (mod(me, 2) == 0) then
            neighbor = me - 1
        else
            neighbor = me + 1
        end if

        if (mod(me, 2) == 0) then
            do i = 1, size(x)
                x(i)%flag = 'flat'
                x(i)%sum = -1.0
                x(i)%values(:) = [(-j, j = 1, size(x(i)%values))]
            end do
        else
            do i = 1, size(x)
                x(i)%flag = reverse
                x(i)%sum = -1.0
                x(i)%values(:) = [(i*j*me, j = 1,size(x(i)%values))]
            end do
        end if

        sync images(neighbor) ! this avoids the sync all
    end subroutine
end module

program assumed_size_dt002
use m
use m1, only : x, initialize_x
use iso_fortran_env, only: error_unit
    implicit none

    integer me, np
    integer i, j
    logical, external :: precision_r8

    me = this_image()
    np = num_images()

    if (me == 1) then
        print *, 'start running'
    end if

    call initialize_x

    sync memory

    call reverse_order_n_sum(x, 10)

    sync memory

    ! now it's time to check for the values of x
    do i = 1, size(x)
        if (mod(me,2) == 0) then ! there is no reversal
            if ((x(i)%flag /= 'flat') .or. &
                (.not. precision_r8(x(i)%sum, -1.0d0)) .or. &
                (any(x(i)%values(:) /= [(-j, j = 1, 100)]))) then

                write(error_unit, *) 'verification for x(i) failed: i = ', &
                    i, '; state of x(i) =',x(i)%flag, x(i)%sum,x(i)%values(:)

                write(error_unit, *) 'expected values for x(i)=', &
                    'flat', -1.0d0, [(-j, j = 1, size(x(i)%values(:)))]

                error stop 1
            end if
        else  !the odd number images summarize the two neighbors
            if ((x(i)%flag /= reverse) .or. &
                (.not. precision_r8(x(i)%sum, (i*me-1)*1.0d0*5050))) then

                write(error_unit, *) 'verification for x(i) failed: i = ', &
                    i, '; state of x(i) =',x(i)%flag, x(i)%sum

                write(error_unit, *) 'expected values for x(i)=', &
                    reverse, (i*me-1)*1.0d0*5050

                error stop 2
            end if

            do j = 1, 100
                if (x(i)%values(j) /= i*me*(101-j)) then
                    write(error_unit, *) 'x(i)%values(j) verification: ', &
                        'i,j =',[i,j], '; x(i)%values(j) = ', x(i)%values(j)
                    write(error_unit, *) 'expected val for x(i)%values(j) = ', &
                        i*(101-j) * me

                    error stop 3
                end if
            end do
        end if
    end do

    sync all

    if (me == 1) then
        print *, 'finished'
    end if
end
