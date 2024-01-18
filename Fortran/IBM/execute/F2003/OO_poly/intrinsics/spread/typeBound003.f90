! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/18/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Cross testing type bound.
!*    Polymorphic
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

    function spreadMe(this, i)
        class(AbstractParent), intent(in) :: this
        integer, intent(in) :: i
        class(AbstractParent), pointer :: spreadMe(:)
        allocate(spreadMe(i), SOURCE=spread(this, 1, i))
    end function
end module

program typeBound003
use m
    class(*), allocatable :: b1
    allocate(b1, SOURCE=Base(2))

    select type(b1)
        type is (Base)
            select type(name1=>b1%spreadMe(4))
                type is (Base)
                    print *, "Base", name1
                    print *, size(name1)
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(3,4))

    select type(b1)
        type is (Child)
            select type(name1=>b1%spreadMe(8))
                type is (Child)
                    print *, "Child", name1
                    print *, size(name1)
                    print *, shape(name1)
                class default
                    error stop 3_4
            end select
        class default
            error stop 4_4
    end select
end
