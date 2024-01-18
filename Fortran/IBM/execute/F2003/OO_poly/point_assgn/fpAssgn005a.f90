! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly-pointer's dynamic
!*                               types; use PASS binding to verify results)
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
        integer*4 :: id = 10

        contains

        procedure, pass :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name = ''

        contains

        procedure, pass :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, 'id = ', b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        call printBase(b)

        print *, 'name = ', b%name
    end subroutine
end module

program fpAssgn005a
use m

    class (base), pointer :: b_ptr
    class (child), pointer :: c_ptr

    type (base), target :: b1
    type (child), target :: c1

    c1 = child (20, name = 'c2')

    c_ptr => c1

    b_ptr => c_ptr%base

    call b_ptr%print  !! base type

    call c_ptr%print  !! child type

    b_ptr => c1%base

    call b_ptr%print  !! base type

    b_ptr => b1

    call b_ptr%print  !! base type

    allocate (c_ptr)

    c_ptr%id = 30
    c_ptr%name = 'c_ptr'

    b_ptr => c_ptr

    call b_ptr%print    !! child type

    b_ptr => c_ptr%base

    call b_ptr%print    !! base type

    b_ptr => c_ptr

    deallocate (b_ptr)
end
