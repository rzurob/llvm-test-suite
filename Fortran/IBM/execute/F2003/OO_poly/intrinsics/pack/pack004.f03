!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/10/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : ARRAY is poly. MASK is scalar or array.
!                              VECTOR is present.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent
        integer i
    end type

    type, extends(AbstractParent) :: Base
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program pack004
use m
    class(AbstractParent), pointer :: a1(:,:)

    class(AbstractParent), pointer :: e1
    class(AbstractParent), pointer :: e2
    class(AbstractParent), allocatable :: e3

    class(AbstractParent), allocatable :: v1(:)

    allocate(e1, SOURCE=Child(1,-1))
    allocate(e2, SOURCE=Child(2,-2))
    allocate(e3, SOURCE=Child(3,-3))

    allocate(a1(2,3), SOURCE=reshape((/e1,e2,e3,e1,e2,e3/),(/2,3/)))

    allocate(v1(7), SOURCE=(/(Child(i,-i),i=11,17)/))

    select type(name1=>pack(a1, .TRUE., v1))
        class is (AbstractParent)
            print *, "A"
        type is (Base)
            print *, "B", name1
        type is (Child)
            print *, "C", name1
        class default
            error stop 1_4
    end select

    select type(name1=>pack(a1, MOD(a1%i,2)==1, v1))
        class is (AbstractParent)
            print *, "A"
        type is (Base)
            print *, "B", name1
        type is (Child)
            print *, "C", name1
        class default
            error stop 2_4
    end select
end
