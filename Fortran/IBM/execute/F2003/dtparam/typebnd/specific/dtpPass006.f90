
module arrayTemp
    type realArray (n)
        integer, len :: n

        real(8), private :: data(n)

        contains

        procedure :: sort => orderArray
        procedure :: update => updateArray
        procedure :: get => getArray
    end type

    integer, parameter :: sortingUP = 1
    integer, parameter :: sortingDOWN = -1

    contains

    subroutine updateArray (at, array)
        class(realArray(*)), intent(out) :: at
        real(8), intent(in) :: array(at%n)

        at%data = array
    end subroutine

    real(8) function getArray (at)
        class(realArray(*)), intent(in) :: at

        dimension getArray(at%n)

        getArray = at%data
    end function

    subroutine orderArray (at, direction)
        class(realArray(*)), intent(inout) :: at
        integer, intent(in) :: direction

        select case (direction)
            case (sortingUP)
                call sortReal8Up (at%data, at%n)

            case (sortingDOWN)
                call sortReal8Down (at%data, at%n)

            case default
                stop 100

        end select
    end subroutine
end module

module randomize
    type, private :: randomGen (n)
        integer, len :: n

        integer, private :: counter
        real, private :: data(n)
        integer, private :: indices(n)
    end type

    type(randomGen(:)), allocatable, protected :: rand

    contains

    subroutine setupRand (n)
        integer, intent(in) :: n

!        rand = randomGen (n)(0, [(i, i=1,n)])
        if (allocated(rand)) deallocate(rand)

        allocate (randomGen (n) :: rand)
        rand%indices =  [(i, i=1,n)]
        rand%counter = n

        call random_number (rand%data)
    end subroutine

    integer function nextIndex ()
        integer, save :: local_int(50000)
        real, save :: local_real(50000)
        integer n

        if (allocated(rand)) then
            n = rand%counter

            if (n < 0) stop 11

            itemp = 1 + n*rand%data(1)

            nextIndex = rand%indices(itemp)

            local_real(:n-1) = rand%data(2:n)
            local_int(:n-1) = [rand%indices(1:itemp-1), rand%indices(itemp+1:n)]

            rand%data(:n-1) = local_real(:n-1)
            rand%indices(:n-1) = local_int(:n-1)
            rand%counter = n-1
        else
            stop 10
        end if
    end function
end module

program dtpPass006
use arrayTemp
use randomize
    type(realArray(:)), allocatable :: aT1

    integer, parameter :: arraySize = 50000

    real(8), allocatable :: d1(:)

    real(8) d2(arraySize)

    logical(4), external :: precision_r8

    !! d1 is ordered ascendingly
    d1 = log([(i*1.2d-2, i=1,50000)])

    call setupRand (arraySize)

    !! set the array d2 as d1 in a random order
    do i = 1, arraySize
        d2(i) = d1(nextIndex())
    end do

    allocate (realArray(arraySize) :: aT1)

    !! set the aT1's data to be d2
    call aT1%update(d2)

    !! sort it descendingly
    call aT1%sort(sortingDOWN)

    !! verify the results
    d1 = aT1%get()

    if (size(d1) /= arraySize) error stop 1_4

    do i = 1, arraySize
        if (.not. precision_r8(d1(i), log(1.2d-2*(arraySize+1-i)))) error stop 2_4
    end do

    !!! test part 2
    call setupRand(arraySize/2)

    !! set d2(101:arraySize/2+100) so that it contains element d1(1:arraySize/2)
    do i = 1, arraySize/2
        d2(100+i) = d1(nextIndex())
    end do

    deallocate(aT1)

    allocate (realArray(arraySize/2) :: aT1)

    call aT1%update(d2(101:))

    !! now sort it ascendingly
    call aT1%sort(sortingUP)

    !! verify the results
    d1 = aT1%get()

    if (size(d1) /= arraySize/2) error stop 3_4

    do i = 1, arraySize/2
        if (.not. precision_r8 (d1(i), log(1.2d-2*(arraySize/2+i)))) error stop 4_4
    end do
end

