!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 01/27/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : ARRAY or SHIFT is array element or array
!                              section. Poly and unlimited poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program array001
use m
    class(Base), allocatable :: b1(:,:)
    class(*), pointer :: u1(:,:)
    integer :: i1(5,5)

    allocate(b1(4,5), SOURCE=reshape((/(Child(i,-i),i=1,20)/),(/4,5/)))
    allocate(u1(3,4), SOURCE=reshape((/(Base(i),i=1,12)/),(/3,4/)))
    i1 = reshape((/(i,i=1,25)/), (/5,5/))

    select type(name1=>cshift(b1(2:3,2:4), i1(3, 2), 2))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>cshift(u1(:,2:4), i1(2:4,2)))
        type is (Base)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
