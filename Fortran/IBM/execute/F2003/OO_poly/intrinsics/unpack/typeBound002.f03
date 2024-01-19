!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Cross testing type bound. Unlimited poly.
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
        procedure :: unpackMe
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    function unpackMe(this, v1, m1)
        class(AbstractParent), intent(in) :: this
        class(AbstractParent), pointer :: v1(:)
        logical, intent(in) :: m1(:,:)
        class(AbstractParent), pointer :: unpackMe(:,:)
        associate(name1=>unpack(v1, m1, this))
            allocate(unpackMe(size(name1,1),size(name1,2)), &
             SOURCE=name1)
        end associate
    end function
end module

program typeBound002
use m
    class(*), pointer :: b1
    class(AbstractParent), pointer :: v1(:)
    logical :: m1(5,5)

    allocate(b1, SOURCE=Base(2))
    allocate(v1(8), SOURCE=(/(Base(i),i=1,8)/))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE., &
     .FALSE.,.TRUE./), (/5,5/), (/.FALSE.,.TRUE./))

    select type(b1)
        class is (AbstractParent)
            select type(name1=>b1%unpackMe(v1, m1(2:4,2:5)))
                type is (Base)
                    print *, "Base", name1
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select

    deallocate(b1,v1)
    allocate(b1, SOURCE=Child(3,4))
    allocate(v1(8), SOURCE=(/(Child(i,-i),i=101,108)/))

    select type(b1)
        class is (AbstractParent)
            select type(name1=>b1%unpackMe(v1, m1(2:,:4)))
                type is (Child)
                    print *, "Child", name1
                    print *, shape(name1)
                class default
                    error stop 3_4
            end select
        class default
            error stop 4_4
    end select
end
