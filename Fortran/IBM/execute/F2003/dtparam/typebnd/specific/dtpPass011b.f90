! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/11/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (Test case on assumed type
!                               parameters in type bound functions; overriding
!                               binding.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

use m1
    class(base(:)), pointer :: b1, b2
    type(child(:)), allocatable :: c1

    type(child(20)) c2

    type, extends(base) :: child2
        complex(8) cx(n)
    end type


    !! test child type's bindings
    c2%ids = [(i, i = 1, 20)]

    c2%cx = [(cmplx(dsin (i*1.0d-1), dcos(i*1.0d-1), 8), i = 1, 20)]

    allocate(child(15) :: c1)

    do i = 15, 1, -1
        c1%cx(i) = cmplx(dsin (i*1.0d-1), dcos(i*1.0d-1), 8)

        c1%ids(i) = i
    end do

    associate (x => c1%equal(c2))
        if (size(x) /= 20) error stop 1_4

        if (any(x .neqv. [(.true., i = 1, 15), (.false., i = 1, 5)])) &
                error stop 2_4
    end associate

    !! test poly-data
    allocate (b1, source = c1)

    allocate (b2, source = b1)

    if (any(b1%equal(b2) .neqv. .true.)) error stop 3_4

    c1%cx(:13:2) = c1%cx(2::2)

    if (any(b1%equal(c1) .neqv. [(.false., .true., i = 1, 7), .true.])) &
            error stop 4_4

    deallocate (b2)

    allocate (b2, source=child2(15)(b1%ids, c1%cx))

    if (any(b1%equal(b2) .neqv. .false.)) error stop 5_4
end
