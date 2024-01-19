! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
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
    type, ABSTRACT :: base
        contains

        procedure (printBase), pass (b), deferred :: print
    end type

    type, extends (base) :: child
        integer*4 :: id

        contains

        procedure :: print => printChild
    end type

    interface
        subroutine printBase (b)
        import base
            class (base), intent(in) :: b
        end subroutine
    end interface

    contains

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id
    end subroutine

    class (base) function produceBasePtr (b)
        class (base), intent(in) :: b
        pointer produceBasePtr

        allocate (produceBasePtr, source=b)
    end function
end module

program ffuncRet005
use m
    class (base), pointer :: b

    b => produceBasePtr (child (10))

    call b%print

    deallocate (b)
end
