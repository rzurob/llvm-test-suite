!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 02/02/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : ARRAY is a structure component, which
!                              is a scalar. The object containing the
!                              component is an array and is poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        type(Base) :: b1
        integer j
    end type
end module

program structureComponent005
use m
    class(AbstractParent), allocatable :: c1(:,:,:)

    allocate(c1(2,2,2), SOURCE=reshape((/(Child(i,Base(-i),-i), &
     i=101,108)/), (/2,2,2/)))

    select type(c1)
        type is (Child)
            associate(name1=>cshift(c1%b1, reshape((/1,2,3,4/), &
             (/2,2/)), 3))
                if(.NOT. same_type_as(name1, Base(1))) error stop 1_4
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            end associate
        class default
            error stop 2_4
    end select
end
