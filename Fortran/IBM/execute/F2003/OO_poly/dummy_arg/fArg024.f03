! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (constraint C1233)
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
        integer*4 :: id
    end type

    contains

    subroutine test1 (b)
        class (base), intent(inout), volatile :: b(:)
    end subroutine

    subroutine test2 (b)
        type (base), intent(inout), volatile :: b(:)
    end subroutine
end module

program fArg024
use m
    class (base), pointer :: b_ptr1(:)
    type (base), pointer :: b_ptr2 (:)

    allocate (b_ptr1(10), b_ptr2(5))

    call test1 (b_ptr1)
    call test1 (b_ptr2)

    call test2 (b_ptr1)
    call test2 (b_ptr2)
end