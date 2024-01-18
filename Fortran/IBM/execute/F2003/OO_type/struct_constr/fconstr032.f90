! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (poly-allocatable
!*                               component in structure constructor)
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

        contains

        procedure, nopass :: typeID => baseTypeID

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        integer*4 :: id = 1

        contains

        procedure, nopass :: typeID => childTypeID
        procedure :: print => printChild
    end type

    contains

    integer*4 function baseTypeID ()
        baseTypeID = 1
    end function

    integer*4 function childTypeID ()
        childTypeID = 2
    end function

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, 'empty type'
    end subroutine
end module

module m1
use m
    type container
        class (base), allocatable :: component
    end type
end module

program fconstr032
use m1
    class (base), allocatable :: b1
    type (container) :: co1 = container(null())

    if (allocated (co1%component)) error stop 1_4

    if (co1%component%typeID () /= 1) error stop 2_4

    allocate (child :: b1)

    co1 = container (component = b1)

    if (co1%component%typeID() /= 2) error stop 3_4

    call co1%component%print

    deallocate (b1)

    allocate (b1, source = child (10))

    co1 = container (b1)

    if (co1%component%typeID() /= 2) error stop 4_4

    call co1%component%print
end
