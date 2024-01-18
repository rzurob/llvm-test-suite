!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/05/2005
!*
!*  DESCRIPTION                : data pointer assignment (C716 type
!                               compatibility for assignment to happen)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn001d1
    type base
        integer id
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    type (base), pointer :: b1
    class (child), pointer :: c1 => null()

    b1 => c1    !<-- illegal
end
