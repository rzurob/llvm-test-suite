! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/pack/merge002.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/20/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Function return of pack is TSOURCE or
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
    class(AbstractParent(:,4)), pointer :: c1(:,:)
    class(*), pointer :: b1(:,:)
    class(*), pointer :: v1(:)
    logical :: m1(8)

    allocate(c1(4,2), SOURCE=reshape((/(Child(20,4)(i,-i),i=1,8)/),(/4,2/)))
    allocate(b1(3,5), SOURCE=reshape((/(Child(20,4)(i,i+1),i=1,15)/),(/3,5/)))
    allocate(v1(8), SOURCE=(/(Child(20,4)(i,-i),i=101,108)/))
    m1 = (/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./)

    select type(name1=>merge(pack(c1, MOD(c1%i,2)==1, v1), &
     pack(b1, reshape(m1,(/3,5/),(/.TRUE.,.FALSE./))), m1))
        type is (Child(*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
