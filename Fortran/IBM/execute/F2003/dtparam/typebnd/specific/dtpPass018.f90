! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/17/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               type bound procedure (A test case uses the
!                               abstract data type and a object container
!                               containing this type; test sorting algorithm)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module dataTypeMod
    type, abstract :: dataType (k)
        integer, kind :: k = 4

        contains

        procedure(printData), deferred :: print

        procedure(compare), deferred :: lessThan
        generic :: operator(<) => lessThan

        procedure (assgnDATA), deferred :: assgn
        generic :: assignment(=) => assgn
    end type

    abstract interface
        subroutine printData (dt)
        import
            class(dataType), intent(in) :: dt
        end subroutine

        subroutine assgnDATA (dt1, dt2)
        import
            class(dataType), intent(out) :: dt1
            class(dataType), intent(in) :: dt2
        end subroutine

        logical function compare (dt1, dt2)
        import
            class(dataType), intent(in) :: dt1, dt2
        end function
    end interface
end module dataTypeMod

module arrayTempMod
use dataTypeMod
    type :: arrayTemp (k)
        integer, kind :: k = 4

        class(dataType(k)), allocatable :: data(:)

        contains

        procedure :: sort => sortAT
    end type

    contains

    !! we use a bubble sort algorithm
    subroutine sortAT (at)
        implicit none
        class(arrayTemp), intent(inout) :: at

        class (dataType(at%k)), allocatable :: temp
        integer i, j

        if (.not. allocated(at%data)) stop 10

        do i = size(at%data), 1, -1
            do j = 1, i
                if (at%data(i) < at%data(j)) then
                    allocate (temp, source=at%data(j))

                    at%data(j) = at%data(i)
                    at%data(i) = temp

                    deallocate (temp)
                end if
            end do
        end do
    end subroutine
end module arrayTempMod

module dataMod
use dataTypeMod
    type, extends(dataType) :: dataArray (n)
        integer, len :: n

        integer(k) :: values(n)

        contains

        procedure :: print => printDataArray
        procedure :: lessThan => dt1LessThanDt2
        procedure :: assgn => assgnDataArray
    end type

    contains

    subroutine printDataArray (dt)
        class(dataArray(n=*)), intent(in) :: dt

        print *, dt%values, 'sum = ', sum(dt%values)
    end subroutine

    logical function dt1LessThanDt2 (dt1, dt2)
        class(dataArray(n=*)), intent(in) :: dt1
        class(dataType), intent(in) :: dt2

        select type (dt2)
            type is (dataArray (n=*))
                dt1LessThanDt2 = sum(dt1%values) < sum(dt2%values)

            class default
                stop 20
        end select
    end function

    subroutine assgnDataArray (dt1, dt2)
        class(dataArray(n=*)), intent(out) :: dt1
        class(dataType), intent(in) :: dt2

        select type (dt2)
            type is (dataArray (n=*))
                if (dt2%n /= dt1%n) stop 25

                dt1%values = dt2%values

            class default
            stop 30
        end select
    end subroutine
end module dataMod

program dtpPass018
use dataMod
use arrayTempMod
    implicit none
    type(arrayTemp) at1

    integer i, n, arraySize

    n = 1024

    arraySize = 1024

    allocate (dataArray(n=n):: at1%data(arraySize))

    do i = 1, arraySize
        at1%data(i) = dataArray(n=n)(arraySize-i)
    end do

    call at1%sort

    !! now verify the sorted results of at1
    select type (x => at1%data)
        type is (dataArray(n=*))
            do i = 1, arraySize
                if (any(x(i)%values /= i-1)) error stop 1_4

                if (sum(x(i)%values) /= (i-1) * n) error stop 2_4
            end do

        class default
            error stop 3_4
    end select
end
