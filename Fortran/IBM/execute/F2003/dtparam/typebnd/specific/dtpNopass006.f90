! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/02/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound procedure (Same procedure
!                               bounds to different types)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module utils
    real :: stores(5000)

    contains

    real function average (r1, r2)
        real, intent(in) :: r1(:)
        real, intent(out), allocatable :: r2(:)

        if (size(r1) == 0) return

        average = sum(r1) /size(r1)

        allocate (r2(size(r1)))

        do i = 1, size(r2)
            r2(i) = (r1(i) - average)**2
        end do
    end function
end module

module realData
use utils

    type realTemp (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)

        contains

        procedure, nopass :: calc4 => average
    end type
end module

module statistics
use utils, only: average

    type dataType (n)
        integer, len :: n = 150

        real :: data(n)

        contains

        procedure, nopass :: average
    end type
end module


program dtpNopass006
use statistics, only: dataType
use realData, only: realTemp
use utils, only: stores

    class(dataType(:)), allocatable :: d1, d2(:)
    type(realTemp(4,:)), pointer :: r1
    type(realTemp(8,10)) r2, r3

    real, allocatable :: rArray(:)

    real(8), parameter :: pi = 3.1415926d0

    logical, external :: precision_test

    !! test procedure calls via dataType
    allocate (dataType(1000) :: d1)

    do i = 1, 1000
        x = pi/1000*i

        d1%data(i) = sin(x)
    end do

    if (.not. precision_test (d1%average(r2=rArray, r1=d1%data), 0.63662_4, &
        5.0e-5)) error stop 1_4

    call calculateVals (d1%n)

    do i = 1, 1000
        if (.not. precision_test (rArray(i), stores(i), 1.0e-4)) then
            print *, i, rArray(i), stores(i)
            error stop 2_4
        end if
    end do

    !! test procedure calls via realTemp
    allocate (realTemp(4,5000) :: r1)

    do i = 1, r1%n
        x = pi/r1%n*i

        r1%data(i) = sin(x)
    end do

    if (.not. precision_test (r3%calc4(r1%data, rArray), 0.63662_4, &
        5.0e-5)) error stop 3_4

    call calculateVals (r1%n)

    do i = 1, r1%n
        if (.not. precision_test (rArray(i), stores(i), 1.0e-4)) then
            print *, i, rArray(i), stores(i)

            error stop 4_4
        end if
    end do
end


logical function precision_test (a, b, relErr)
    real, intent(in) :: a, b, relErr

    real, parameter :: nealZero = 2.e-6

    if ((abs(a) < nealZero) .and. (abs(b) < nealZero) .and. (a*b >= 0)) then
        precision_test = .true.

        return
    end if

    precision_test = abs(a-b)/abs(a+b)/2.0 <= relErr
end function

subroutine calculateVals(i)
use utils, only: stores
    integer, intent(in) :: i

    real(8), parameter :: pi = 3.1415926d0

    real localAvg

    if ((i > size(stores)) .or. (i <= 0)) stop 100

    localAvg = 0

    do j = 1, i
        x = pi*j/i

        stores(j) = sin(x)

        localAvg = localAvg + stores(j)
    end do

    localAvg = localAvg / (j-1)

    do j = 1, i
        stores(j) = (stores(j) - localAvg) * (stores(j) - localAvg)
    end do
end subroutine
