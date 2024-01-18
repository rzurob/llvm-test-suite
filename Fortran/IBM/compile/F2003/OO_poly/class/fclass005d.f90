! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/25/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (poly-pointer with PROTECTED
!                               attribute; it can not be allocated or pointer
!                               assign to a target)
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
        integer*4 id
    end type

    class (base), pointer, protected :: b_ptr

    contains

    subroutine allocateB_ptr
        allocate (b_ptr)
    end subroutine

    subroutine reallocPtr (p)
        class (base), pointer :: p

        if (associated(p)) deallocate(p)

        allocate (p)
    end subroutine
end module

program fclass005d
use m
    call allocateB_ptr

    call reallocPtr (b_ptr)

    call abc
end

subroutine abc
use m
    type (base), target, save :: b1

    allocate (b_ptr)    !! illegal

    b_ptr => b1         !! illegal

    b_ptr%id = 10       !! legal
end subroutine
