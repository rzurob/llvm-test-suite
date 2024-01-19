!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : ARRAY, SHIFT, or BOUNDARY is an associate
!                              name.
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

program associate001
use m
    class(AbstractParent), pointer :: ap1(:,:) => null()
    integer :: i1(5,5)
    class(AbstractParent), allocatable :: b1(:,:)

    allocate(ap1(4,5),SOURCE=reshape((/(Child(i,-i),i=1,20)/),(/4,5/)))
    i1 = reshape((/(i,i=1,25)/), (/5,5/))
    allocate(b1(3,3),SOURCE=reshape((/(Child(i,i+1),i=11,19)/),(/3,3/)))

    associate(name1=>ap1(2:,:4),name2=>i1(2:4,1),name3=>b1(2,:))
        select type(name3=>eoshift(name1,name2,name3,2))
            type is (Child)
                print *, name3
                print *, size(name3)
                print *, shape(name3)
            class default
                error stop 1_4
        end select
    end associate
end
