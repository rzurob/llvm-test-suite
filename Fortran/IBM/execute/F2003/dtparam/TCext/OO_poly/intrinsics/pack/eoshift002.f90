! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/pack/eoshift002.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : Function return of pack is ARRAY of
!                              eoshift. Poly and unlimited poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(AbstractParent) :: Base    ! (20,4)
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type
end module

program eoshift002
use m
    class(AbstractParent(:,4)), pointer :: c1(:,:)
    class(*), pointer :: v1(:)

    allocate(c1(4,2), SOURCE=reshape((/(Child(20,4)(i,-i),i=1,8)/),(/4,2/)))
    allocate(v1(8), SOURCE=(/(Child(20,4)(i,-i),i=101,108)/))

    select type(name1=>eoshift(pack(c1,MOD(c1%i,2)==0,v1),2,c1(2,2)))
        type is (Child(*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
