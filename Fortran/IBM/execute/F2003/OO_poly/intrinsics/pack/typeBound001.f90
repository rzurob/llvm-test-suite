!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
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
        procedure :: packMe
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    function packMe(this, i, m1)
        class(AbstractParent), intent(in) :: this
        integer, intent(in) :: i
        logical, intent(in) :: m1(i,i)
        class(AbstractParent), pointer :: packMe(:)
        class(AbstractParent), pointer :: temp(:,:)
        allocate(temp(i,i), SOURCE=this)
        select type(name1=>pack(temp, m1))
            type is (Base)
                allocate(packMe(size(name1)), SOURCE=name1)
            type is (Child)
                allocate(packMe(size(name1)), SOURCE=name1)
            class default
                error stop 3_4
        end select
    end function
end module

program typeBound001
use m
    class(AbstractParent), pointer :: b1
    logical :: m1(5,5)

    allocate(b1, SOURCE=Base(2))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE., &
     .FALSE.,.TRUE./), (/5,5/), (/.FALSE.,.TRUE./))

    select type(name1=>b1%packMe(3,m1(2:4,2:4)))
        type is (Base)
            print *, "Base", name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(3,4))

    select type(name1=>b1%packMe(4,m1(2:,2:)))
        type is (Child)
            print *, "Child", name1
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
