! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/unpack/cshift001.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : VECTOR, MASK, or FIELD is function return
!                              of cshift. Poly and unlimited poly.
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

program cshift001
use m
    class(AbstractParent(:,4)), allocatable :: v1(:)
    class(AbstractParent(:,4)), pointer :: f1(:,:)
    class(*), pointer :: f2(:,:)
    logical :: m1(4,3)

    allocate(v1(8), SOURCE=(/(Child(20,4)(i,-i),i=101,108)/))
    allocate(f1(4,3), SOURCE=reshape((/(Child(20,4)(-i,i),i=1,12)/),(/4,3/)))
    allocate(f2(4,3), SOURCE=reshape((/(Child(20,4)(i,i+1),i=1,12)/),(/4,3/)))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE./), (/4,3/), (/.FALSE.,.TRUE./))

    select type(name1=>unpack(cshift(v1,-3), &
     cshift(m1,(/-1,1,-2,2/),2), cshift(f1,(/1,-2,-1/),1)))
        type is (Child(*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>unpack(cshift(v1,2), &
     cshift(m1,(/1,-2,3/)), cshift(f2,(/-1,2,-3,4/),2)))
        type is (Child(*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
