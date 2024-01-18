! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (allocated allocatable subojects in
!                               intrinsic assignment: NOTE it is possible to
!                               observe the order change of the finalization of
!                               two allocatable components, data1 and data2.  If
!                               that happens; update the expected results.)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(4) :: id = -1

        contains

        final :: finalizeBase, finalizeBaseRank1
        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name = 'default'

        contains

        final :: finalizeChild, finalizeChildRank1
        procedure :: print => printChild
    end type

    type container
        class (base), allocatable :: data1
        class (base), allocatable :: data2(:)
    end type

    contains

    subroutine printBase (b)
        class (base), intent (in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine finalizeChild (c)
        type (child) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child) :: c(:)

        print *, 'finalizeChildRank1'
    end subroutine

    subroutine finalizeBase(b)
        type (base), intent (in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent (in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

program ffinal530a
use m
    type (container) :: co1, co2

    allocate (co1%data1, source=child(1, 'co1.data1'))
    allocate (co1%data2 (2:3), source=child(2, 'co1.data2'))
    allocate (child :: co2%data1)
    allocate (co2%data2(5))

    print *, 'first assignment'

    co2 = co1

    if ((lbound(co2%data2,1) /= 2) .or. (ubound(co2%data2,1) /= 3)) error stop 1_4

    call co2%data1%print

    call co2%data2(2)%print
    call co2%data2(3)%print

    print *, 'end'
end
