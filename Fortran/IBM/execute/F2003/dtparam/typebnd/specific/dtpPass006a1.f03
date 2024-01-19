! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :  05/29/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               A test case for testing randomization routines
!                               used in dtpPass006.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module randomize
    type, private :: randomGen (n)
        integer, len :: n

        real, private :: data(n)
        integer, private :: indices(n)

        contains

        procedure :: initialize => initializeRandom
    end type

    type(randomGen(:)), allocatable, protected :: rand

    contains

    subroutine setupRand (n)
        integer, intent(in) :: n

        allocate (randomGen(n) :: rand)

        call rand%initialize
    end subroutine

    subroutine initializeRandom (r1)
        class(randomGen(*)), intent(inout) :: r1

        r1%indices = [(i, i=1,r1%n)]

        call random_number (r1%data)
    end subroutine

    integer function nextIndex ()
        if (allocated(rand) .and. (rand%n > 0)) then
            itemp = 1 + rand%n*rand%data(1)

            nextIndex = rand%indices(itemp)

            rand = randomGen(rand%n-1)([rand%data(2:)],&
                [rand%indices(1:itemp-1), rand%indices(itemp+1:)])
        else
            stop 10
        end if
    end function
end module

program dtpPass006a1
use randomize
    call setupRand (300)

    !! this loop prints 1 to 300 in random order (if the random_number() gives
    !truely random values :-)
    do i = 1, 300
        print *, nextIndex ()
    end do
end

