! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr052.f
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
    type base(k1)    ! (8)
        integer, kind :: k1
        integer(k1)      id

        contains

        procedure :: getID => getBaseID
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: getName => getChildName
    end type

    contains

    integer(8) function getBaseID (b)
        class (base(8)), intent(in) :: b

        getBaseID = b%id
    end function

    character function getChildName (c)
        class(child(8,1,*)), intent(in) :: c

        dimension getChildName(len(c%name))

        do i = 1, size(getChildName)
            getChildName(i) = c%name(i:i)
        end do
    end function
end module

program fconstr052
use m
    type(base(8)), parameter :: defaultBase = base(8)(id = -1)
    type(child(8,1,20)), parameter :: defaultChild = child(8,1,20) (name='un-named', base=defaultBase)

    character(2), dimension(20) :: names

    if (defaultBase%getID() /= -1) error stop 1_4

    if (defaultChild%getID() /= -1) error stop 2_4

    names = defaultChild%getName()

    print *, names
end
