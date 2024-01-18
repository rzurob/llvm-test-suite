! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=none -qdeferredlp /tstdev/OO_poly/intrinsics/null/selectType002.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 03/02/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : Return of null is the selector of a
!                              select type construct. MOLD is poly.
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

program selectType002
use m
    class(AbstractParent(:)), pointer :: ap1(:,:) => null()
    allocate(ap1(4,5),SOURCE=reshape((/(Child(20,4,4)(i,-i),i=1,20)/),(/4,5/)))

    select type(name1=>null(ap1))
        type is (Child(*,4,4))
            error stop 1_4
        class is (Base(*,4))
            error stop 2_4
        class is (AbstractParent(*))
            ! should come here, but do nothing
        class default
            error stop 3_4
    end select
end
