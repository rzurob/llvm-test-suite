! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/18/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
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
        procedure :: spreadMe
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    subroutine spreadMe(this, i)
        class(AbstractParent), intent(in) :: this
        integer, intent(in) :: i
        associate(name1=>spread(this, 1, i))
            select type(name1)
                type is (Base)
                    print *, "Base", name1
                    print *, size(name1)
                    print *, shape(name1)
                type is (Child)
                    print *, "Child", name1
                    print *, size(name1)
                    print *, shape(name1)
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

    b1%i = 1
    c1%i = 2
    c1%j = 3

    call b1%spreadMe(10)
    call c1%spreadMe(8)
end
