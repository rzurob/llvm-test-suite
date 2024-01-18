!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/24/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Function return of unpack is the SOURCE
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
        integer i
    end type

    type, extends(AbstractParent) :: Base
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program spread002
use m
    class(*), pointer :: v1(:)
    class(AbstractParent), pointer :: f1(:,:)

    allocate(v1(8), SOURCE=(/(Child(i,-i),i=101,108)/))
    allocate(f1(4,2), SOURCE=reshape((/(Child(i,-i),i=1,8)/),(/4,2/)))

    select type(name1=>spread(unpack(v1, (MOD(f1%i,2)==0 .OR. &
     MOD(f1%i,3)==2), f1), 2, 3))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
