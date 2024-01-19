! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/04/2007
!*
!*  DESCRIPTION                : dtparam (defined assignment)
!                               This tests the assumed length type parameters
!                               used in a defined assignment subroutine).
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n = 3

        character(n), allocatable :: name

        contains

        procedure :: assgn => assgnB1B2
        generic :: assignment(=) => assgn
    end type

    contains

    !! this routine override the intrinsic assignment behavior for type base
    subroutine assgnB1B2 (b1, b2)
        class(base(*)), intent(inout) :: b1
        class(base(*)), intent(in) :: b2

        if (allocated(b2%name)) then
            b1%name = b2%name
        end if
    end subroutine
end module

program definedAssgn001
use m
    type(base(10)) b1
    type(base(:)), pointer :: b2

    b1%name = 'xlftest team'

    b1 = base(20)(null())

    if (.not. allocated(b1%name)) error stop 1_4
    if (b1%name /= 'xlftest te') error stop 2_4

    deallocate (b1%name)

    b1 = base(5) ('test101')

    if (.not. allocated(b1%name)) error stop 3_4
    if (b1%name /= 'test1') error stop 4_4

    allocate (base(5):: b2)

    b2 = b1

    if (.not. allocated(b2%name)) error stop 5_4
    if (len(b2%name) /= 5) error stop 6_4

    if (b2%name /= 'test1') error stop 7_4
end
