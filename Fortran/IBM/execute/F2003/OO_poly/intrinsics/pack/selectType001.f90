!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : ARRAY is an associate name of a select
!                              type construct. Associate name is non
!                              poly.
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
        integer j
    end type
end module

program selectType001
use m
    class(AbstractParent), pointer :: ap1(:,:) => null()
    class(*), allocatable :: v1(:)
    logical :: m1(5,5)

    allocate(ap1(4,5),SOURCE=reshape((/(Child(i,-i),i=1,20)/),(/4,5/)))
    allocate(v1(10),SOURCE=(/(Child(i,i+1),i=11,20)/))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE., &
     .FALSE.,.TRUE./), (/5,5/), (/.FALSE.,.TRUE./))

    select type(name1=>ap1(2:,:4))
        type is (Child)
            associate(name2=>pack(name1,m1(:3,2:),v1))
                if(.NOT. same_type_as(name2, Child(1,1))) error stop 1_4
                print *, name2
                print *, shape(name2)
            end associate
        class default
            error stop 2_4
    end select
end
