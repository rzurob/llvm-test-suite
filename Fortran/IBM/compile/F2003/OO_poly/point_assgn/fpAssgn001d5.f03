! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2005
!*
!*  DESCRIPTION                : data pointer assignment (use of object without
!                               pointer attribute in places that require a
!                               pointer attribute)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
    end type

    type, extends(base) :: child
        character(20), allocatable :: name
    end type
end module

program fpAssgn001d5
use m
    class(base), pointer :: b1, b2

    allocate (child:: b1, b2)

    b1%id => null()             !<-- illegal
    nullify (b1%id)             !<-- illegal
    deallocate (b2%id)          !<-- illegal

    select type (b1)
        class is (base)
            b1 => b2            !<-- illegal
            nullify (b1)        !<-- illegal
            deallocate (b1)     !<-- illegal
    end select
end
