!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/05/2005
!*
!*  DESCRIPTION                : data pointer assignment (C716 type
!                               compatibility between POINTER and TARGET)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        contains

        procedure, nopass :: typeID => baseID
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure, nopass :: typeID => childID
    end type

    contains

    integer*4 function baseID()
        baseID = 0
    end function

    integer*4 function childID ()
        childID = 1
    end function
end module

program fpAssgn001d
use m

    class (base), pointer :: b_ptr
    class (child), pointer :: c_ptr
    class (*), pointer :: x

    integer, target :: i
    type (base), target :: b1
    type (child), target :: c1

    c_ptr => b1         !<-- illegal

    c_ptr => b_ptr      !<-- illegal

    c_ptr => c1%base    !<-- illegal

    c_ptr => x          !<-- illegal

    b_ptr => i          !<-- illegal
end
