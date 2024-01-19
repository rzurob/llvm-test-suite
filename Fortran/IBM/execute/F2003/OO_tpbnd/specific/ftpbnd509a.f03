! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type-bound (external procedure as type
!*                               bound; pass binding; overriding binding as
!*                               external procedure)
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

        contains

        procedure :: print => printBase
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine
end module

module m1
use m
    type, extends (base) :: child
        character*20 :: name = ''

        contains

        procedure :: print => printChild
    end type

    private printChild

    interface
        subroutine printChild (b)
        import child
            class (child), intent(in) :: b
        end subroutine
    end interface
end module

program ftpbnd509a
use m1
    type (base), target :: b1
    type (child), target :: c1

    class (base), pointer :: b_ptr


    c1 = child (20, 'c1_test')
    b1 = base (10)

    call c1%print

    b_ptr => c1

    call b_ptr%print

    b_ptr => b1

    call b_ptr%print
end

subroutine printChild (b)
use m1
    class (child), intent(in) :: b

    print *, b%id, b%name
end subroutine
