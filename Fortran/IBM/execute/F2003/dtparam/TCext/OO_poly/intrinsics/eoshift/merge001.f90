! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=none -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/merge001.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : ARRAY is function return of merge.
!                              Poly and unlimited poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(n1)    ! (20)
        integer, len :: n1
    end type

    type, extends(AbstractParent) :: Base(k1)    ! (20,4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2)    ! (20,4,4)
        integer, kind :: k2
        integer(k2)      j
    end type
end module

program merge001
use m
    class(AbstractParent(:)), pointer :: c1(:,:)
    class(*), pointer :: b1(:,:)
    logical :: m1(4,2)

    allocate(c1(4,2), SOURCE=reshape((/(Child(20,4,4)(i,-i),i=1,8)/),(/4,2/)))
    allocate(b1(4,2), SOURCE=reshape((/(Child(20,4,4)(i,i+1),i=1,8)/),(/4,2/)))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE./), (/4,2/))

    select type(name1=>eoshift(merge(c1,b1,m1), (/2,-2/), &
     (/Child(20,4,4)(11,-11),Child(20,4,4)(12,-12)/)))
        type is (Child(*,4,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
