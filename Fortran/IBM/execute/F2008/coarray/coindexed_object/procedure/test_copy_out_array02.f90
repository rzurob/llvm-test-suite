! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-11-22
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : a test on coindexed object being copied out in
!                               function calls.
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

module work_mod
    implicit none
    type worker
        contains

        procedure, nopass :: sum => sumArray
    end type

    contains

    subroutine sumArray (N,arr)
        integer, intent(in) :: n
        real(8), intent(inout) :: arr(-1:n)

        integer i

        arr(-1:0) = 0

        do i = 1, n
            arr(0) = arr(0) + arr(i)
        end do
        arr(-1) = sum(arr(1:n))
    end subroutine
end module

module data_mod
    implicit none

    integer, parameter :: nsize = 1000

    real(8), save :: my_arr(-1:nsize)[*]
end module

program test_copy_out_array02
    use work_mod, only: worker
    use data_mod, only: my_arr, nsize

    implicit none

    integer me, np, i, left
    integer, allocatable, dimension(:) :: seeds
    integer :: seed_size
    type(worker) the_operator
    logical, external :: precision_r8

    me = this_image()
    np = num_images()

    if (np < 2) stop 'program needs at least 2 images to run'

    call random_seed(size = seed_size)

    allocate (seeds(seed_size))

    call random_seed(get = seeds)

    seeds = seeds*me
    call random_seed(put = seeds)

    if (me == 1) then
        my_arr(1:) = [(i, i = 1, nsize)]
    else
        call random_number(my_arr(1:))
    end if

    sync all

    if (me == 1) then
        left = np
    else
        left = me -1
    end if

    call the_operator%sum(nsize, my_arr(:)[left])

    sync all

    if (me == 1) then
        if (.not. precision_r8 (my_arr(-1), my_arr(0))) then
            print *, 'test1 failed'
            print *, my_arr(-1), 'vs', my_arr(0)
            error stop 1
        end if

        if (.not. precision_r8 (my_arr(-1), 0.5d0*nsize*(nsize+1))) then
            print *, 'test2 failed'
            print *, my_arr(-1), 'vs', 0.5d0*nsize*(nsize+1)
            error stop 1
        end if
    else
        if (.not. precision_r8 (my_arr(-1), my_arr(0))) then
            print *, 'test3 failed'
            print *, my_arr(-1), 'vs', my_arr(0)
            error stop 1
        end if

        ! randome number generator precision test: 1000 data points, error is
        ! 3.2% (3.2e-2); to account compuational noises, let's make the error
        ! range to be 4% (4.0d-2).
        print *, abs(my_arr(-1) -5.d2), 'vs', 4.d-2*500
        if (abs(my_arr(-1)-5.d2) > 500.0d0*4.0d-2) then
            print *, 'test4 failed'

            print *, my_arr(-1), 'vs', 500d0, 'not within 4%'
            error stop 1
        end if
    end if
end
