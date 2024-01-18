! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-07-15
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : a simple test case on assumed-size coarray
!                               dummy.  Also exercise the data exchange by
!                               pairs of images. An even number of images is
!                               required to run this test.  Coindexed scalar
!                               object access is tested.
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

    type data_holder
        real(8) :: v(20)
        character(10) :: desc
    end type

    type work_d
        type(data_holder) :: d

        logical :: flag
    end type

    contains

    ! this routine does a data swap between two neighboring images
    ! it is required the number of images must be even.  So the swap is to
    ! exchange data between image 1 and 2, between 3 and 4, so on
    subroutine exchange_data (n, x)
        integer, intent(in) :: n
        type(work_d), dimension(*), codimension[*], intent(inout) :: x

        integer me, np, neighbor, i
        type(work_d) tmp

        me = this_image()
        np = num_images()

        if (mod(np, 2) /= 0) then
            stop 100
        end if

        !! set up the neighbors to exchange data
        if (mod(me, 2) == 0) then
            neighbor = me - 1
        else
            neighbor = me + 1
        end if

        !! now let's exchange the data
        do i = 1, n
            tmp = x(i)[neighbor]
            sync images (neighbor)
            x(i) = tmp
        end do
    end subroutine

    !! routine to initialize the array using assumed-size coarray dummy
    subroutine initialize_data (x, n)
        integer, intent(in) :: n
        type(work_d), intent(out) :: x(*)[*]

        integer :: me, np, i,j, neighbor

        np = num_images()
        me = this_image()

        if (mod(np, 2) /= 0) then
            stop 100
        end if

        if (mod(me, 2) == 0) then
            neighbor = me - 1
        else
            neighbor = me + 1
        end if

        !! set up the values of x
        do i = 1, n
            x(i)%flag = mod(me, 2) == 0
            write (x(i)%d%desc,*) me,i

            x(i)[neighbor]%d%v = [(j*1.0d0*neighbor + i, j = 1, size(x(i)%d%v))]
        end do

        sync images(neighbor)
    end subroutine
end module

program assumed_size_dt003
    use m
    use iso_fortran_env, only: error_unit

    implicit none
    type(work_d), save :: arr(10)[*]
    integer arr_size, me, i, j, neighbor

    integer img_me, idx

    logical, external :: precision_r8

    me = this_image()

    arr_size = 10
    call initialize_data (arr, arr_size)

    !! now let's verify the initialized data
    do i = 1, arr_size
        if (arr(i)%flag .neqv. (mod(me,2) == 0)) then
            write(error_unit, *) 'verification flag fails on image', me,&
                '; expected: ',mod(me,2) == 0, ', actual:',arr(i)%flag

            error stop 1
        end if

        read (arr(i)%d%desc, *) img_me, idx

        if ((img_me /= me) .or. (idx /= i)) then
            write(error_unit, *) 'verification of desc fails on image',me, &
                'element', i, '; Actual: ', img_me, idx

            error stop 2
        end if

        do j = 1, 20
            if (.not. precision_r8(arr(i)%d%v(j), j*1.0d0*me+i*1.0d0)) then
                write (error_unit, *) 'verification arr(i)%d%v(j) fails on image', &
                    me, ', [i,j] =',i,j, ', expected: ',j*1.0d0*me+i*1.0d0, &
                    ', actual: ', arr(i)%d%v(j)

                error stop 3
            end if
        end do
    end do

    print *, 'now exchange data'

    call exchange_data (10, arr)

    if (mod(me, 2) == 0) then
        neighbor = me - 1
    else
        neighbor = me + 1
    end if

    sync images(neighbor)

    !! now let's verify the exchanged data
    do i = 1, arr_size
        if (arr(i)%flag .neqv. (mod(me,2) == 1)) then
            write(error_unit, *) 'verification flag fails on image', me,&
                '; expected: ',mod(me,2) == 1, ', actual:',arr(i)%flag

            error stop 11
        end if

        read (arr(i)%d%desc, *) img_me, idx

        if ((img_me /= neighbor) .or. (idx /= i)) then
            write(error_unit, *) 'verification of desc fails on image',neighbor, &
                'element', i, '; Actual: ', img_me, idx

            error stop 12
        end if

        do j = 1, 20
            if (.not. precision_r8(arr(i)[neighbor]%d%v(j), j*1.0d0*me+i*1.0d0)) then
                write (error_unit, *) 'verification arr(i)[neighbor]%d%v(j) fails on image', &
                    me, ', [i,j] =',i,j, ', expected: ',j*1.0d0*me+i*1.0d0, &
                    ', actual: ', arr(i)[neighbor]%d%v(j)

                error stop 13
            end if
        end do
    end do
end
