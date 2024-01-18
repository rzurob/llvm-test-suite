! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/21/2007
!*
!*  DESCRIPTION                : derived type paramater
!                               modules for definitions of dataType and
!                               flagedData, and their associated procedures.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType (k)
        integer, kind :: k

        real(k) :: data

        contains

        procedure :: lessThan => d1LessThanD2
        procedure :: print => printDataType

        generic :: operator (<) => lessThan
    end type

    contains

    logical function d1LessThanD2 (d1, d2)
        class(dataType(8)), intent(in) :: d1, d2
        implicit none

        d1LessThanD2 = d1%data < d2%data
    end function

    subroutine printDataType (data)
        class(dataType(8)), intent(in) :: data

        write (*, '(g14.5)') data%data
    end subroutine
end module

module m1
use m
    type, extends(dataType) :: flagedData
        logical flag

        contains

        procedure :: isSet => isFlageSet
        procedure :: print => printFlagedData
    end type

    contains

    logical function isFlageSet (fd)
        class(flagedData(8)), intent(in) :: fd

        isFlageSet = fd%flag
    end function

    subroutine printFlagedData (data)
        class (flagedData(8)), intent(in) :: data

        write (*, '(g14.5, L3)') data%data, data%flag
    end subroutine

    subroutine sortFlagedDataArrayDown (d1)
        type(flagedData(8)), intent(inout) :: d1(:)

        type (flagedData(8)) temp

        do i = 2, size(d1)
            temp = d1 (i)

            j = i

            do while ((j > 1) .and. (d1(j-1) < temp))
                d1(j) = d1(j-1)

                j = j - 1
            end do

            d1(j) = temp
        end do
    end subroutine
end module

