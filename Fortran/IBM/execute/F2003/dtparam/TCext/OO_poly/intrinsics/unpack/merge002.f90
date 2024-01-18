! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/unpack/merge002.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

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

program merge002
use m
    class(*), pointer :: v1(:)
    class(AbstractParent(:,4)), pointer :: f1(:,:)
    class(*), pointer :: f2(:,:)
    logical :: m1(8)

    allocate(v1(8), SOURCE=(/(Child(20,4)(i,-i),i=101,108)/))
    allocate(f1(4,2), SOURCE=reshape((/(Child(20,4)(i,-i),i=1,8)/),(/4,2/)))
    allocate(f2(4,2), SOURCE=reshape((/(Child(20,4)(i,i+1),i=1,10)/),(/4,2/)))
    m1 = (/.TRUE.,.FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./)

    select type(name1=>merge(unpack(v1, MOD(f1%i,2)==1, f1), &
     unpack(v1, reshape(m1,(/4,2/),(/.TRUE.,.FALSE./)), f2), &
     reshape(m1,(/4,2/),ORDER=(/2,1/))))
        type is (Child(*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
