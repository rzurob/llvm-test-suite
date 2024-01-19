! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type-bound (external procedure as type
!*                               bound; pass binding; a simpliest test case)
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

    interface
        subroutine printBase (b)
        import base
            class (base), intent(in) :: b
        end subroutine
    end interface
end module

program ftpbnd509
use m
    type (base), target :: b1
    class (base), pointer :: b_ptr

    b_ptr =>  b1

    b_ptr%id = 100

    call b1%print

    call b_ptr%print
end

subroutine printBase (b)
use m, only : base
    class (base), intent(in) :: b

    print *, b%id
end subroutine
