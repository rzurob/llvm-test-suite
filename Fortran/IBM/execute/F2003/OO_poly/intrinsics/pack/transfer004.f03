!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/18/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : Function return of pack is the SOURCE
!                              of transfer. Poly and unlimited poly.
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

program transfer004
use m
    class(AbstractParent), pointer :: c1(:,:,:)
    class(*), pointer :: b1(:)
    class(*), pointer :: v1(:)

    allocate(c1(2,2,2), SOURCE=reshape((/(Child(i,-i),i=1,8)/), &
     (/2,2,2/)))
    allocate(b1(8), SOURCE=(/(Base(i),i=101,108)/))
    allocate(v1(6), SOURCE=(/(Child(i,-i),i=11,16)/))

    select type(name1=>transfer(pack(c1, MOD(c1%i,2)==0, v1), b1))
        type is (Base)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end