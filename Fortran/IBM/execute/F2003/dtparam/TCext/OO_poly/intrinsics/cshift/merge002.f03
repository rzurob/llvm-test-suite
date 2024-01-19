! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/merge002.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 02/01/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : Function return of cshift is TSOURCE or
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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type
end module

program merge002
use m
    class(AbstractParent(4,:)), pointer :: c1(:,:)
    class(*), pointer :: b1(:,:)
    logical :: m1(4,2)

    allocate(c1(4,2), SOURCE=reshape((/(Child(4,20)(i,-i),i=1,8)/),(/4,2/)))
    allocate(b1(4,2), SOURCE=reshape((/(Child(4,20)(i,i+1),i=1,8)/),(/4,2/)))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE., &
     .FALSE.,.TRUE.,.FALSE./), (/4,2/))

    select type(name1=>merge(cshift(c1,(/-2,3/)),cshift(b1,-5),m1))
        type is (Child(4,*))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
