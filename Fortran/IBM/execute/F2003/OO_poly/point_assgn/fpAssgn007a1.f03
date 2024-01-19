! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (nopass binding can be
!*                               called for poly pointer; associated or
!*                               disassociated)
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

        procedure, nopass :: printType => printBaseType
    end type

    type, extends(base) :: child
        contains

        procedure, nopass :: printType => printChildType
    end type

    contains

    subroutine printBaseType
        print *, 'base'
    end subroutine

    subroutine printChildType
        print *, 'child'
    end subroutine
end module

program fpAssgn007a1
use m
    type (base), pointer :: p1

    type (child), pointer :: c_ptr
    type (child), target :: c1

    class (base), pointer :: b_ptr
    class (child), pointer :: c_ptr1

    call p1%printType       !<-- base

    b_ptr => c1

    call b_ptr%printType    !<-- child

    p1 => b_ptr

    call p1%printType       !<-- base

    allocate (c_ptr1)

    b_ptr => c_ptr1

    call b_ptr%printType    !<-- child

    deallocate (c_ptr1)

    b_ptr => c_ptr1

    call b_ptr%printType    !<-- base

    allocate (c_ptr)

    deallocate (c_ptr)

    b_ptr => c_ptr

    call b_ptr%printType    !<-- base
end

