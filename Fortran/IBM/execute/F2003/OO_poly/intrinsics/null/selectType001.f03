!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 03/01/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : Return of null used in the selector of
!                              select type construct.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
        class(Base), pointer :: b
    end type
end module

program selectType001
use m
    class(Base), pointer :: b1

    associate(name1=>Child(1,2,null()))
        select type(name2=>name1%b)
            type is(Base)
                b1 => name2
                if(associated(b1)) error stop 1_4
            class default
                error stop 2_4
        end select
    end associate
end
