!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/04/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : ARRAY is function return of spread.
!                              Poly and unlimited poly.
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

program spread001
use m
    class(AbstractParent), pointer :: c1(:,:)
    class(*), pointer :: b1(:,:)

    allocate(c1(4,2), SOURCE=reshape((/(Child(i,-i),i=1,8)/),(/4,2/)))
    allocate(b1(3,5), SOURCE=reshape((/(Base(i),i=1,15)/),(/3,5/)))

    select type(name1=>eoshift(spread(c1,3,3), &
     reshape((/1,-2,2,-1,3,-1/),(/2,3/)), &
     reshape((/(Child(i,i+1),i=11,16)/),(/2,3/)), 1))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>eoshift(spread(b1,3,2), &
     reshape((/-1,2,-3,4,-2,1/),(/3,2/)), &
     reshape((/(Base(i),i=11,16)/),(/3,2/)), 2))
        type is (Base)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
