! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 1/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (polymorphic pointer
!*                               assigned to extended data type; use nopass
!*                               binding to verify)
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

        procedure, nopass :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name

        contains

        procedure, nopass :: print => printChild
    end type

    contains

    subroutine printBase
        print *, 'base'
    end subroutine

    subroutine printChild
        print *, 'child'
    end subroutine
end module

program fpAssgn001
use m

    class (base), pointer :: b_ptr

    type (child), target :: c1 = child('c1')
    type (base), target :: b1 = base ()

    b_ptr => c1

    if (.not. associated (b_ptr, c1)) error stop 1_4

    call b_ptr%print

    b_ptr => b1

    if ((.not. associated(b_ptr)) .or. associated(b_ptr, b1)) error stop 2_4

    call b_ptr%print
end
