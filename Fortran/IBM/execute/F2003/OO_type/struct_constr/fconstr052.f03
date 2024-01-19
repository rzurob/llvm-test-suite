! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/21/2005
!*
!*  DESCRIPTION                : structure constructor (used in initialization
!                               expression for named constants)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8) id

        contains

        procedure :: getID => getBaseID
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure :: getName => getChildName
    end type

    contains

    integer(8) function getBaseID (b)
        class (base), intent(in) :: b

        getBaseID = b%id
    end function

    character function getChildName (c)
        class(child), intent(in) :: c

        dimension getChildName(len(c%name))

        do i = 1, size(getChildName)
            getChildName(i) = c%name(i:i)
        end do
    end function
end module

program fconstr052
use m
    type(base), parameter :: defaultBase = base(id = -1)
    type(child), parameter :: defaultChild = child (name='un-named', base=defaultBase)

    character(2), dimension(20) :: names

    if (defaultBase%getID() /= -1) error stop 1_4

    if (defaultChild%getID() /= -1) error stop 2_4

    names = defaultChild%getName()

    print *, names
end
