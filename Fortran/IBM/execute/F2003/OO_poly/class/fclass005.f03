! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (poly-pointer with PROTECTED
!*                               attribute)
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

        contains

        procedure :: print => printBase
        procedure, non_overridable :: setID => setBaseID
    end type

    class (base), pointer, protected :: b_ptr

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine setBaseID (b, i)
        class (base), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = i
    end subroutine

    subroutine allocateB_ptr
        allocate (b_ptr)
    end subroutine
end module

program fclass005
use m
    call allocateB_ptr

    call b_ptr%setID (10)

    call b_ptr%print

end