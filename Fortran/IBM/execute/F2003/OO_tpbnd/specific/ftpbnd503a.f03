! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (correct binding invocation
!*                               with overriding binding called in inherited
!*                               binding)
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

        procedure, pass, non_overridable :: print => printBase
        procedure, nopass :: printHeader => printBaseHeader
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        call b%printHeader

        print *, 'id = ', b%id
    end subroutine

    subroutine printBaseHeader
        print *, 'base type'
    end subroutine
end module

module m1
use m
    type, extends (base) :: child

        contains

        procedure, nopass :: printHeader => printChildHeader
    end type

    contains

    subroutine printChildHeader

        print *, 'child type'
    end subroutine
end module

program ftpbnd503a
use m1
    class (base), pointer :: b1
    type (child), target :: c1 = child(10)

    class (child), pointer :: c_ptr

    call c1%base%print

    call c1%print

    b1 => c1
    c_ptr => c1

    call b1%print

    call c_ptr%print

    call c_ptr%base%print
end