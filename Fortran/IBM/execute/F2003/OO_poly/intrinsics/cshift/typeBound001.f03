!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 02/02/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : Cross testing type bound. Polymorphic.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent
        contains
        procedure :: cshiftMe
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    function cshiftMe(this, i, j)
        class(AbstractParent), intent(in) :: this
        integer, intent(in) :: i
        integer, intent(in) :: j
        class(AbstractParent), pointer :: cshiftMe(:)
        class(AbstractParent), pointer :: temp(:)
        allocate(temp(i), SOURCE=this)
        allocate(cshiftMe(i), SOURCE=cshift(temp, j))
    end function
end module

program typeBound001
use m
    class(AbstractParent), pointer :: b1
    allocate(b1, SOURCE=Base(2))

    select type(name1=>b1%cshiftMe(4,-3))
        type is (Base)
            print *, "Base", name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(3,4))

    select type(name1=>b1%cshiftMe(8,5))
        type is (Child)
            print *, "Child", name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end