! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of temporary created by
!*                               function call in call statement)
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

        final :: finalizeBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild

        procedure :: makeObj => replicateChildObj

        final :: finalizeChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    type (child) function replicateChildObj (c)
        class (child), intent(in) :: c

        replicateChildObj%id = c%id
        replicateChildObj%name = c%name
    end function

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine
end module

program ffinal514a4
use m
    interface
        subroutine printVal (b)
        use m
            class (base), intent(in) :: b
        end subroutine
    end interface

    type (child) :: c1 = child (10, 'c1_static')

    call printVal (c1%makeObj())

end


subroutine printVal (b)
use m
    class (base), intent(in) :: b

    call b%print
end subroutine
