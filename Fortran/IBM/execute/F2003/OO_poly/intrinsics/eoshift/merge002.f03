!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : Function return of eoshift is TSOURCE or
!                              FSOURCE of merge. Poly and unlimited
!                              poly.
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

program merge002
use m
    class(AbstractParent), pointer :: c1(:,:)
    class(*), pointer :: b1(:,:)
    logical :: m1(4,2)

    allocate(c1(4,2), SOURCE=reshape((/(Child(i,-i),i=1,8)/),(/4,2/)))
    allocate(b1(4,2), SOURCE=reshape((/(Child(i,i+1),i=1,8)/),(/4,2/)))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE., &
     .FALSE.,.TRUE.,.FALSE./), (/4,2/))

    select type(name1=>merge(eoshift(c1,(/-2,1/), &
     (/Child(11,-11),Child(12,-12)/)), eoshift(b1,2, &
     (/Child(13,-13),Child(14,-14)/)),m1))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
