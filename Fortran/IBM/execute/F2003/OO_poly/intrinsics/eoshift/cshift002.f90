!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : Function return of eoshift is ARRAY of
!                              cshift. Poly and unlimited poly.
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
    end type
end module

program cshift002
use m
    class(AbstractParent), pointer :: c1(:,:)
    class(*), pointer :: b1(:,:)

    allocate(c1(4,2), SOURCE=reshape((/(Child(i,-i),i=1,8)/),(/4,2/)))
    allocate(b1(4,2), SOURCE=reshape((/(Child(i,-i),i=1,8)/),(/4,2/)))

    select type(name1=>cshift(eoshift(c1,(/-2,1/), &
     (/Child(11,-11),Child(12,-12)/)), (/-1,2,3,-4/), 2))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>cshift(eoshift(b1,(/-2,1/), &
     (/Child(11,-11),Child(12,-12)/)), (/-1,2,3,-4/), 2))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
