! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/unpack/spread001.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/24/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : VECTOR, MASK, or FIELD is function return
!                              of spread. Poly and unlimited poly.
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

program spread001
use m
    class(AbstractParent(:,4)), pointer :: v1
    class(*), pointer :: f1(:,:)
    logical :: m1(2,2)

    allocate(v1, SOURCE=Child(20,4)(101,-101))
    allocate(f1(4,2), SOURCE=reshape((/(Child(20,4)(i,-i),i=1,8)/),(/4,2/)))
    m1 = reshape((/.TRUE.,.FALSE.,.FALSE.,.TRUE./), (/2,2/))

    select type(name1=>unpack(spread(v1,1,9), &
     spread(m1,1,4), spread(f1,3,2)))
        type is (Child(*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
