! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/20/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Cross testing type bound.
!*    Non-poly
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type, abstract :: AbstractParent
        contains
        procedure :: transferToMe
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    subroutine transferToMe(this, a)
        class(AbstractParent), intent(in) :: this
        class(AbstractParent), intent(in) :: a
        associate (name1=>transfer(a, this))
            select type(name1)
                type is (Base)
                    print *, "Base", name1
                type is (Child)
                    print *, "Child", name1%i
                class default
                    error stop 1_4
            end select
        end associate
    end subroutine
end module

program typeBound001
use m
    type(Base) :: b1
    type(Child) :: c1

    call b1%transferToMe(Child(4,4))
    call c1%transferToMe(Base(8))
end
