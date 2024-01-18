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
!*  DATE                       : 06/22/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               a file containing 2 modules used in test case
!                               dtpPass012a
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module dataArray
use m
    type arrayType (n)
        integer, len :: n

        type(dataType(8)) data(n)

        contains

        procedure :: topN => selectLargestNval
        procedure :: print => printArrayType
    end type

    contains

    subroutine printArrayType (aT1)
        class (arrayType(*)), intent(in) :: aT1

        do i = 1, aT1%n
            call aT1%data(i)%print
        end do
    end subroutine

    function selectLargestNval (aT1, n)
        class(arrayType(*)), intent(in) :: aT1
        integer, intent(in) :: n

        type(arrayType(n)) selectLargestNval

        real(8) localArray(aT1%n)
        external sortReal8Down

        if (aT1%n < n) stop 100

        localArray = aT1%data%data

        call sortReal8Down (localArray, size(localArray))

        selectLargestNval%data(:)%data = localArray(:n)
    end function
end module


module flagedDataArray
use m1
    type flagedArrayType (n)
        integer, len :: n

        type (flagedData(8)) data(n)

        contains

        procedure :: topN => selectLargestNval
        procedure :: print => printFlagedArrayType
    end type

    contains

    subroutine printFlagedArrayType (ft1)
        class (flagedArrayType(*)), intent(in) :: ft1

        do i = 1, ft1%n
            call ft1%data(i)%print
        end do
    end subroutine

    function selectLargestNval (ft1, n)
        class (flagedArrayType(*)), intent(in) :: ft1
        integer, intent(in) :: n

        type (flagedArrayType(n)) selectLargestNval

        type(flagedData(8)) local(ft1%n)

        if (ft1%n < n) stop 150

        local = ft1%data

        call sortFlagedDataArrayDown (local)

        selectLargestNval  = flagedArrayType(n)(flagedData(8) &
                (-huge(1.0_8), .false.))

        itotal = 0

        do i = 1, size(local)
            if (local(i)%isSet()) then
                itotal = itotal + 1

                selectLargestNval%data(itotal) = local(i)
            end if

            if (itotal >= n) exit
        end do
    end function
end module

