! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/unpack/cshift002.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Function return of unpack is ARRAY of
!                              cshift. Poly and unlimited poly.
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

    type, extends(AbstractParent) :: Base(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
    end type

    type, extends(Base) :: Child(n3,k3)    ! (20,4,4,20,20,4)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)      j
    end type
end module

program cshift002
use m
    class(*), pointer :: v1(:)
    class(AbstractParent(:,4)), pointer :: f1(:,:)

    allocate(v1(8), SOURCE=(/(Child(20,4,4,20,20,4)(i,-i),i=101,108)/))
    allocate(f1(4,2), SOURCE=reshape((/(Child(20,4,4,20,20,4)(i,-i),i=1,8)/),(/4,2/)))

    select type(name1=>cshift(unpack(v1, MOD(f1%i,2)==1, f1), (/2,-1/)))
        type is (Child(*,4,4,*,*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
