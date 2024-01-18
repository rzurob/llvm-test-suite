!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/24/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Function return of unpack is TSOURCE or
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
        integer i
    end type

    type, extends(AbstractParent) :: Base
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program merge002
use m
    class(*), pointer :: v1(:)
    class(AbstractParent), pointer :: f1(:,:)
    class(*), pointer :: f2(:,:)
    logical :: m1(8)

    allocate(v1(8), SOURCE=(/(Child(i,-i),i=101,108)/))
    allocate(f1(4,2), SOURCE=reshape((/(Child(i,-i),i=1,8)/),(/4,2/)))
    allocate(f2(4,2), SOURCE=reshape((/(Child(i,i+1),i=1,10)/),(/4,2/)))
    m1 = (/.TRUE.,.FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./)

    select type(name1=>merge(unpack(v1, MOD(f1%i,2)==1, f1), &
     unpack(v1, reshape(m1,(/4,2/),(/.TRUE.,.FALSE./)), f2), &
     reshape(m1,(/4,2/),ORDER=(/2,1/))))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
