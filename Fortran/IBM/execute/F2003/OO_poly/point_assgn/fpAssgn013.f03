! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/12/2004
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

    type (child), target :: c1 = child('c1')
    type (base), target :: b1 = base ()

    class (base), pointer :: bm_ptr

    contains

    subroutine printBase
        print *, 'base'
    end subroutine

    subroutine printChild
        print *, 'child'
    end subroutine
end module

program fpAssgn013
use m

    class (base), pointer :: b_ptr

    b_ptr => c1

    if (.not. associated (b_ptr, c1)) error stop 1_4

    call b_ptr%print

    b_ptr => b1

    if (associated (b_ptr, b1)) error stop 2_4

    call b_ptr%print

    bm_ptr => c1

    if (.not. associated (bm_ptr, c1)) error stop 3_4

    call bm_ptr%print

    bm_ptr => b1

    if (associated (bm_ptr, b1)) error stop 4_4

    call bm_ptr%print
end