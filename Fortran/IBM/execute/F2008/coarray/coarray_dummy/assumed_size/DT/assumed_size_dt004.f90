! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-07-18
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : A simple test on assumed-size coarray.  Test
!                               access coindexed objects via the dummy.
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

    type data_type
        integer :: flag
        complex(8) :: phase(2)
    end type

    contains

    ! this routine initializes the coarray dummy by image 1
    subroutine initialize_data (x,n)
        type(data_type), intent(out), dimension(*), codimension[*] :: x
        integer, intent(in) :: n

        integer me, np, img, i,j

        me = this_image()
        np = num_images()

        if (me == 1) then
            do img = 2, np
                x(1:n)[img]%flag = [(i*img, i = 1, n)]

                do j = 1, n
                    x(j)[img]%phase(:) = &
                        j*img*[cmplx(1.0d0, 2.0d0, 8), cmplx(1.0d1, 2.0d1, 8)]
                end do
            end do

            sync images(*)

            x(1:n)%flag = [(i, i = 1, n)]
            do j = 1, n
                x(j)%phase(:) = j*[cmplx(1.0d0, 2.0d0, 8), cmplx(1.0d1, 2.0d1, 8)]
            end do
        else
            sync images(1)
        end if
    end subroutine
end module

module data_mod
use m, only: data_type
    type(data_type), save :: arr(10)[*]
end module

program assumed_size_dt004
use m
use data_mod, only: arr
use iso_fortran_env, only: error_unit

    implicit none

    integer arr_size, i, me

    logical, external :: precision_x6

    if (num_images() == 1) then
        write(error_unit, *) 'program requires at least 2 images'
        stop 100
    end if

    arr_size = 10

    call initialize_data (arr, arr_size)

    me = this_image()

    ! now verify the results
    do i = 1, 10
        if (arr(i)%flag /= me*i) then
            write (error_unit, *) 'initialization on image ', me, 'failed', &
                'on element', i, 'arr(i)%falg=',arr(i)%flag,'; expected:', &
                me*i

            error stop 1
        end if

        if ((.not. precision_x6(arr(i)%phase(1),i*me*cmplx(1.0d0,2.0d0,kind=8))) .or. &
            (.not. precision_x6(arr(i)%phase(2),i*me*cmplx(1.0d1,2.0d1,kind=8)))) then
            write (error_unit, *) 'initialization on image ', me, 'failed', &
                'on element', i, 'arr(i)%phase=',arr(i)%phase,'; expected:', &
                me*i*[cmplx(1.0d0, 2.0d0), cmplx(1.0d1, 2.0d1)]

            error stop 2
        end if
    end do
end
