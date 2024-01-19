! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/intrinsics/pack/pack004.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/10/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : ARRAY is poly. MASK is scalar or array.
!                              VECTOR is present.
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

program pack004
use m
    class(AbstractParent(:,4)), pointer :: a1(:,:)

    class(AbstractParent(:,4)), pointer :: e1
    class(AbstractParent(:,4)), pointer :: e2
    class(AbstractParent(:,4)), allocatable :: e3

    class(AbstractParent(:,4)), allocatable :: v1(:)

    allocate(e1, SOURCE=Child(20,4,4,20,20,4)(1,-1))
    allocate(e2, SOURCE=Child(20,4,4,20,20,4)(2,-2))
    allocate(e3, SOURCE=Child(20,4,4,20,20,4)(3,-3))

    allocate(a1(2,3), SOURCE=reshape((/e1,e2,e3,e1,e2,e3/),(/2,3/)))

    allocate(v1(7), SOURCE=(/(Child(20,4,4,20,20,4)(i,-i),i=11,17)/))

    select type(name1=>pack(a1, .TRUE., v1))
        class is (AbstractParent(*,4))
            print *, "A"
        type is (Base(*,4,4,*))
            print *, "B", name1
        type is (Child(*,4,4,*,*,4))
            print *, "C", name1
        class default
            error stop 1_4
    end select

    select type(name1=>pack(a1, MOD(a1%i,2)==1, v1))
        class is (AbstractParent(*,4))
            print *, "A"
        type is (Base(*,4,4,*))
            print *, "B", name1
        type is (Child(*,4,4,*,*,4))
            print *, "C", name1
        class default
            error stop 2_4
    end select
end
