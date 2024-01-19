! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/13/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the case where dummy-arg is the expr for
!                               the intrinsic assignment; use assumed-shape
!                               array for intrinsic types and derived type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, pointer :: ids(:) => null()
    end type

    type, extends(base) :: child
        character(7), allocatable :: name
    end type

    contains

    subroutine copyString (c1, c2, lb)
        character(:), allocatable :: c1(:)
        character(*), intent(in) :: c2(lb:)
        integer, intent(in) :: lb

        c1 = c2
    end subroutine

    subroutine allocString (s, c)
        character(*), allocatable :: s(:)
        character(*) c(0:)

        s = c
    end subroutine

    subroutine copyBase (b1, b2, lb)
        type(base), allocatable, intent(inout) :: b1(:)
        class(base), intent(in) :: b2(lb:)
        integer, intent(in) :: lb

        b1 = b2
    end subroutine
end module

program dummyArg002
use m
    character(*), parameter :: colors(0:2) = (/'RED   ', 'BLUE  ', 'GREEN '/)

    character(:), allocatable :: str(:), s1*3(:)
    type(base), allocatable :: b1(:)
    class(base), pointer :: b2(:)

    !! test 1: for character types with deferred-length
    call copyString (str, colors, 3)

    if ((.not. allocated(str)) .or. (len(str) /= 6)) error stop 1_4

    if ((lbound(str,1) /= 3) .or. (ubound(str,1) /= 5)) error stop 2_4

    if (any(str /= colors)) error stop 4_4

    call copyString (str, colors(2:0:-1), 0)

    if ((.not. allocated(str)) .or. (len(str) /= 6)) error stop 5_4

    if ((lbound(str,1) /= 3) .or. (ubound(str,1) /= 5)) error stop 6_4

    if (any(str /= (/character(6) :: 'GREEN', 'BLUE', 'RED'/))) error stop 7_4

    !! test 2: for character type with fixed length
    call allocString (s1, (/character(5) :: 'xlftest', 'team', 'work'/))

    if (.not. allocated(s1)) error stop 8_4

    if ((lbound(s1,1) /= 0) .or. (ubound(s1,1) /= 2)) error stop 9_4

    if (any(s1 /= (/'xlf', 'tea', 'wor'/))) error stop 10_4

    call allocString (s1, (/str, colors/))

    if ((lbound(s1,1) /= 0) .or. (ubound(s1,1) /= 5)) error stop 11_4

    if (any(s1 /= (/'GRE', 'BLU', 'RED', 'RED', 'BLU', 'GRE'/))) &
            error stop 12_4


    !! test 3: for derived types
    allocate (b2(10), source=(/(child(null(), 'xlftest'), i=1,10)/))

    do i = 1, 10, 2
        allocate (b2(i)%ids(i), source=(/(j, j=1,i)/))
    end do

    call copyBase(b1, b2, 10)

    if (.not. allocated(b1)) error stop 13_4

    if ((lbound(b1, 1) /= 10) .or. (ubound(b1,1) /= 19)) error stop 14_4

    do i = 10, 19, 2
        if ((.not. associated(b1(i)%ids, b2(i-9)%ids)) .or. &
            associated(b1(i+1)%ids)) error stop 15_4

        do j = 1, i-9
            if (b1(i)%ids(j) /= j) error stop 16_4
        end do
    end do
end

