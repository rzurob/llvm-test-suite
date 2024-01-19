!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 01/27/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : ARRAY or SHIFT is an associate name.
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

    allocate(ap1(4,5), SOURCE=reshape((/(Child(i,-i),i=1,20)/), (/4,5/)))
    i1 = reshape((/(i,i=1,25)/), (/5,5/))

    associate(name1=>ap1(2:,:4), name2=>i1(2,:3))
        select type(name3=>cshift(name1,name2,2))
            type is (Child)
                print *, name3
                print *, size(name3)
                print *, shape(name3)
            class default
                error stop 1_4
        end select
    end associate
end
