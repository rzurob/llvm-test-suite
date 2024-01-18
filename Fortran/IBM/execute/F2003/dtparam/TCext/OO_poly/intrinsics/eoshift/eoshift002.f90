! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=none -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/eoshift002.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : ARRAY is poly. SHIFT is scalar or array.
!                              BOUNDARY is scalar or array, non
!                              poly or poly. DIM is present or not.
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
        integer(k1)   :: i = 8
    end type

    type, extends(Base) :: Child(k2)    ! (20,4,4)
        integer, kind :: k2
        integer(k2)   :: j = 9
    end type
end module

program eoshift002
use m
    class(Base(:,4)), pointer :: b1(:,:,:)
    class(AbstractParent(:)), allocatable :: a1(:,:)

    allocate(b1(3,2,2), SOURCE=reshape((/(Child(20,4,4)(i,-i),i=1,12)/), &
     (/3,2,2/)))
    allocate(a1(2,2),SOURCE=reshape((/(Child(20,4,4)(i,i+1),i=11,14)/),(/2,2/)))

    select type(name1=>eoshift(b1, -1, a1))
        type is (Child(*,4,4))
            print *, name1
        class default
            error stop 1_4
    end select

    select type(name1=>eoshift(b1, 1, Child(20,4,4)(88,-88), 2))
        type is (Child(*,4,4))
            print *, name1
        class default
            error stop 2_4
    end select

    select type(name1=>eoshift(b1,reshape((/1,2,-1,-2/),(/2,2/)),a1,1))
        type is (Child(*,4,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 3_4
    end select
end
