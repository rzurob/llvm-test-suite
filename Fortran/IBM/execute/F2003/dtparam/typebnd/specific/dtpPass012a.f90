!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/22/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound procedure (Test case for
!                               case the type bound is a function of derived
!                               type with type parameter that is a specification
!                               expression; return the largest N element of an
!                               array.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtpPass012a
    call testArrayDataType

    call testArrayFlagedData
end


subroutine testArrayDataType
use dataArray
    implicit none
    type(arrayType(:)), allocatable :: result
    type(arrayType(2000)) ra

    integer n, i

    n = 10

    !! set up the data for ra so that we know what the largest 10 values are
    ra%data(:199)%data = [(sin(i*1.0), i = 1, 199)]
    ra%data(200)%data = 2.0

    ra%data(201:399)%data = 1.05*ra%data(:199)%data
    ra%data(400)%data = 1.5

    ra%data(401:599)%data = ra%data(199:1:-1)%data
    ra%data(600)%data = 3.5

    ra%data(601:799)%data = ra%data(399:201:-1)%data
    ra%data(800)%data = 2.5

    ra%data(801:999)%data = ra%data(201:399)%data
    ra%data(1000)%data = 1.75

    ra%data(1001:1199)%data = [(cos(i*1.0), i = 1, 199)]
    ra%data(1200)%data = 1.25

    ra%data(1201:1399)%data = 1.1 * ra%data(1001:1199)%data
    ra%data(1400)%data = 3.25

    ra%data(1401:1599)%data = ra%data(1399:1201:-1)%data
    ra%data(1600)%data = 2.25

    ra%data(1601:1799)%data = 0
    ra%data(1800)%data = 3.0

    ra%data(1801:1999)%data = 1
    ra%data(2000)%data = 4.0

    result = ra%topN (n)

    call result%print
end subroutine


subroutine testArrayFlagedData
use flagedDataArray
    implicit none
    type (flagedArrayType(:)), pointer :: fa1
    type (flagedArrayType(1000)) fa2

    integer i, n

    n = 10

    fa2%data(:)%data = [real(8) :: (sin (i*1.0), i = 1, 300), &
            (sqrt(i*1.0), i = 1, 400), &
            (cos (i*1.0), i = 1, 300)]

    fa2%data(:)%flag = [(.true., .false., i = 1, 500)]

    allocate (flagedArrayType(n) :: fa1)

    fa1 = fa2%topN(n)

    call fa1%print
end subroutine
