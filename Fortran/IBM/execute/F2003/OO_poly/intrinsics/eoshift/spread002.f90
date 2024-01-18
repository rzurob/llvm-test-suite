!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/04/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : Function return of eoshift is the SOURCE
!                              of spread. Poly and unlimited poly.
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

program spread002
use m
    class(AbstractParent), pointer :: c1(:,:)
    class(*), pointer :: b1(:,:)

    allocate(c1(4,2), SOURCE=reshape((/(Child(i,-i),i=1,8)/),(/4,2/)))
    allocate(b1(3,5), SOURCE=reshape((/(Base(i),i=101,115)/),(/3,5/)))

    select type(name1=>spread(eoshift(c1, (/-2,3/), &
     (/Child(17,-17),Child(18,-18)/)), 3, 2))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>spread(eoshift(b1, (/1,-2,3/), &
     (/Base(1),Base(2),Base(3)/), 2), 1, 2))
        type is (Base)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
