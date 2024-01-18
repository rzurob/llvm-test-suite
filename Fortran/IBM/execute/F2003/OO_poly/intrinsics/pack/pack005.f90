!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/10/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : ARRAY is unlimited poly. MASK is scalar
!                              or array. VECTOR is not present.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent
        integer i
    end type

    type, extends(AbstractParent) :: Base
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program pack005
use m
    class(*), pointer :: a1(:,:)

    class(*), pointer :: e1
    class(*), pointer :: e2
    class(*), allocatable :: e3

    allocate(e1, SOURCE=Child(1,-1))
    allocate(e2, SOURCE=Child(2,-2))
    allocate(e3, SOURCE=Child(3,-3))

    allocate(a1(2,3), SOURCE=reshape((/e1,e2,e3,e1,e2,e3/),(/2,3/)))

    select type(name1=>pack(a1, .TRUE.))
        type is (Child)
            print *, "C", name1
        class default
            error stop 1_4
    end select

    select type(name1=>a1)
        type is (Child)
            associate(name2=>pack(name1, MOD(name1%i,2)==1))
                if(.NOT. same_type_as(name2, Child(1,1))) error stop 2_4
                print *, name2
            end associate
        class default
            error stop 3_4
    end select
end
