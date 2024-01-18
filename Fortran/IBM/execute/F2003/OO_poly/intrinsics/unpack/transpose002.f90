!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/24/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Function return of unpack is the SOURCE
!                              of transpose. Poly and unlimited poly.
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

program transpose002
use m
    class(*), pointer :: v1(:)
    class(AbstractParent), pointer :: f1(:,:)

    allocate(v1(6), SOURCE=(/(Child(i,-i),i=11,16)/))
    allocate(f1(4,3), SOURCE=reshape((/(Child(-i,i),i=1,12)/),(/4,3/)))

    select type(name1=>transpose(unpack(v1, MOD(f1%i,2)==0, f1)))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
