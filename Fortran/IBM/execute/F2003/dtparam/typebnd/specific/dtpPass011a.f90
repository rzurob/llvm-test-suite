!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/11/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (Test case on assumed type
!                               parameters in type bound functions; of different
!                               assumed length values.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


use m
    class(base(:)), allocatable :: b1, b2

    type (base(10)) b3
    type (base(20)) b4
    logical, allocatable :: flags(:)

    allocate (base(15) :: b1)
    allocate (base(10) :: b2)

    b1%ids = [(i, i = 1, 15)]

    b2%ids = [(i, 10*i, i = 1, 10, 2)]
    b3%ids = [(i, i = 1, 10)]

    b4%ids = [(i, 10*i, i = 1, 20, 2)]

    associate (x => b2%equal(b3))
        if (size(x) /= 10) error stop 1_4

        if (any(x .neqv. [(.true., .false., i = 1, 5)])) error stop 2_4
    end associate

    if (any(b2%equal(b4) .neqv. [(.true., i = 1, 10), &
        (.false., i = 1, 10)])) error stop 3_4


    call moreTest (b1, b3, flags)

    if (.not. allocated(flags)) error stop 4_4

    if (size(flags) /= 15) error stop 5_4

    if (any(flags .neqv. [(.true., i = 1, 10), (.false., i = 1, 5)])) &
        error stop 6_4


    call test (b1, b2, flags)

    if (size(flags) /= 15) error stop 7_4

    if (any(flags .neqv. [(.true., .false., i = 1, 5), &
        (.false., i = 1, 5)])) error stop 8_4

    contains


    subroutine test (b11, b12, f)
        class(base(:)), allocatable :: b11, b12
        logical, allocatable :: f(:)

        f = b11%equal(b12)
    end subroutine

    subroutine moreTest (b1, b2, f)
        type(base(*)), intent(in) :: b1, b2
        logical, allocatable :: f(:)

        f = b1%equal(b2)
    end subroutine
end
