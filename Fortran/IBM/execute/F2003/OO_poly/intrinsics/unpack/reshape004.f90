!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/24/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Function return of unpack is the SOURCE
!                              of reshape. Poly and unlimited poly.
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

program reshape004
use m
    class(*), pointer :: v1(:)
    class(AbstractParent), pointer :: c1(:,:,:)

    allocate(v1(10), SOURCE=(/(Child(i,-i),i=101,110)/))
    allocate(c1(2,3,2), SOURCE=reshape((/(Child(i,-i),i=1,12)/), &
     (/2,3,2/)))

    select type(name1=>reshape(unpack(v1, MOD(c1%i,2)==0, c1), &
     (/3,4/), (/Child(-1,-2),Child(-3,-4)/), (/2,1/)))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
