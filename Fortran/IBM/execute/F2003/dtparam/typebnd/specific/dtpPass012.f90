!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 06/13/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (Test case for case the type
!                               bound is a function of derived type with type
!                               parameter.)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type realArray (n)
        integer, len :: n

        real :: data (n)

        contains

        procedure :: top10 => selectLargest10Val
        procedure :: print => printRealArray
    end type

    contains

    subroutine printRealArray (ra)
        class(realArray(*)), intent(in) :: ra

        write (*, '(5g14.5)') ra%data(:)
    end subroutine

    function selectLargest10Val (ra)
        class(realArray(*)), intent(in) :: ra

        class(realArray(10)), allocatable :: selectLargest10Val

        real(8) localArray(ra%n)

        if (ra%n < 10) stop 100

        localArray = ra%data

        call sortReal8Down (localArray, ra%n)

        allocate (selectLargest10Val, source=realArray(10)(localArray(:10)))
    end function
end module


module m1
use m
    type, extends(realArray) :: flagedArray
        logical :: flag(n)

        contains

        procedure :: print => printFlagedArray
        procedure :: top10 => selectLargest10ValWithFlag
    end type

    contains

    subroutine printFlagedArray (ra)
        class(flagedArray(*)), intent(in) :: ra

        write (*, '(5(g14.5, L3))') (ra%data(i), ra%flag(i), i = 1, ra%n)
    end subroutine


    !! use the sorting algorithm defined in sorting.f90: insertion sort
    function selectLargest10ValWithFlag (ra)
        class (flagedArray(*)), intent(in) :: ra

        class (realArray(10)), allocatable :: selectLargest10ValWithFlag

        type(flagedArray(10)) sortedArray

        type pair
            real data
            logical flag
        end type

        type (pair) temp, localArray(ra%n)

        if (ra%n < 10) stop 101

        localArray = [(pair(ra%data(i), ra%flag(i)), i = 1, ra%n)]

        do i = 2, ra%n
            temp = localArray(i)

            j = i

            do while ((j > 1) .and. (localArray(j-1)%data < temp%data))
                localArray(j) = localArray(j-1)

                j = j - 1
            end do

            localArray(j) = temp
        end do

        itotal = 0

        sortedArray = flagedArray(10)(-huge(1.0), .false.)
        !! now select top 10 elements
        do i = 1, ra%n
            if (localArray(i)%flag) then
                itotal = itotal + 1

                sortedArray%data(itotal) = localArray(i)%data
                sortedArray%flag(itotal) = localArray(i)%flag
            end if

            if (itotal >= 10) exit
        end do

        allocate (selectLargest10ValWithFlag, source = sortedArray)
    end function
end module


program dtpPass012
    call testRealArray

    call testFlagedArray
end


subroutine testRealArray
use m
    type(realArray(:)), allocatable :: ra, ra1

    allocate (realArray(2000) :: ra)

    !! set up the data for ra so that we know what the largest 10 values are
    ra%data(:199) = [(sin(i*1.0), i = 1, 199)]
    ra%data(200) = 2.0

    ra%data(201:399) = 1.05*ra%data(:199)
    ra%data(400) = 1.5

    ra%data(401:599) = ra%data(199:1:-1)
    ra%data(600) = 3.5

    ra%data(601:799) = ra%data(399:201:-1)
    ra%data(800) = 2.5

    ra%data(801:999) = ra%data(201:399)
    ra%data(1000) = 1.75

    ra%data(1001:1199) = [(cos(i*1.0), i = 1, 199)]
    ra%data(1200) = 1.25

    ra%data(1201:1399) = 1.1 * ra%data(1001:1199)
    ra%data(1400) = 3.25

    ra%data(1401:1599) = ra%data(1399:1201:-1)
    ra%data(1600) = 2.25

    ra%data(1601:1799) = 0
    ra%data(1800) = 3.0

    ra%data(1801:1999) = 1
    ra%data(2000) = 4.0

    allocate (ra1, source = ra%top10())

    call ra1%print
end subroutine


subroutine testFlagedArray
use m1
    class(realArray(:)), allocatable :: ra, ra1
    type(flagedArray(:)), pointer :: fa

    !! test 1
    allocate (flagedArray(500) :: fa)

    fa%flag = [(.false., .true., i = 1, 250)]

    fa%data = [cos([(i*1.0e-1, i = 1, 200)]), (i*1.0, i = 1, 100), &
               sin([(i*1.0e-1, i = 1, 200)])]

    allocate(ra, source = fa%top10())

    call ra%print

    !! test 2
    allocate (ra1, source=flagedArray(500)(fa%data, &
        [.true., .true., .true., (.false., i = 1, 497)]))

    deallocate (ra)

    allocate (ra, source=ra1%top10())

    call ra%print
end subroutine
