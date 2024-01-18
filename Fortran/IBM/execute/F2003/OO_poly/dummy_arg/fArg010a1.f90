! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (VALUE attribute; pointer
!*                               component won't be changed if re-allocated)
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
        integer*4 :: id = -1

        contains

        procedure, nopass :: typeID => baseID
    end type

    type, extends (base) :: child
        character*20 :: name = 'default'

        contains

        procedure, nopass :: typeID => childID
    end type

    type container
        class (base), pointer :: data => null()
    end type

    contains

    subroutine allocateTemp (c)
        type (container), value :: c

        allocate (child:: c%data)

        if (c%data%typeID () /= 2) error stop 10_4
    end subroutine

    integer*4 function baseID()
        baseID = 1
    end function

    integer*4 function childID ()
        childID = 2
    end function
end module

program fArg010a1
use m
    class (container), allocatable :: c1 (:)

    allocate (c1 (3))

    call allocateTemp (c1(1))

    allocate (c1(2)%data)

    call allocateTemp (c1(2))

    if (associated (c1(1)%data) .or. associated (c1(3)%data)) error stop 1_4

    if (.not. associated (c1(2)%data)) error stop 2_4

    if (c1(2)%data%typeID () /= 1) error stop 3_4

    deallocate (c1(2)%data)
end
