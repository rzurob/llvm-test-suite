!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/20/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : ARRAY is function return of spread.
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

program spread001
use m
    class(AbstractParent), pointer :: c1(:,:)
    class(*), pointer :: b1(:)

    allocate(c1(4,2), SOURCE=reshape((/(Child(i,-i),i=1,8)/),(/4,2/)))
    allocate(b1(14), SOURCE=(/(Child(i,i+1),i=101,114)/))

    select type(name1=>pack(spread(c1,3,2), &
     spread((MOD(c1%i,2)==0 .OR. MOD(c1%i,3)==2),3,2), b1))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
