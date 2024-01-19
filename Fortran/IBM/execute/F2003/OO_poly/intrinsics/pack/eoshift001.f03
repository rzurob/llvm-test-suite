!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : ARRAY is function return of eoshift.
!                              Poly and unlimited poly.
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

program eoshift001
use m
    class(AbstractParent), pointer :: c1(:,:)
    class(*), pointer :: b1(:,:)
    logical :: m1(4,2)

    allocate(c1(4,2), SOURCE=reshape((/(Child(i,-i),i=1,8)/),(/4,2/)))
    allocate(b1(4,2), SOURCE=reshape((/(Child(i,i+1),i=1,8)/),(/4,2/)))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE./), (/4,2/))

    select type(name1=>pack(eoshift(c1,(/1,-2/),(/b1(1,2),b1(3,1)/),1),&
     MOD(c1%i,2)==1))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>pack(eoshift(b1,(/2,-1/),c1(3,:)), m1, &
     reshape(c1,(/8/))))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
