!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/04/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : Function return of eoshift is the SOURCE
!                              of transfer. Poly and unlimited poly.
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

program transfer004
use m
    class(AbstractParent), pointer :: c1(:,:,:)
    class(*), pointer :: b1(:)

    allocate(c1(2,2,2), SOURCE=reshape((/(Child(i,-i),i=1,8)/), &
     (/2,2,2/)))
    allocate(b1(8), SOURCE=(/(Base(i),i=101,108)/))

    select type(name1=>transfer(eoshift(c1, reshape((/1,-2,2,-1/), &
     (/2,2/)), reshape((/(Child(i,i+1),i=11,14)/), (/2,2/)), 3), &
     eoshift(b1, -5, Base(-8), 1)))
        type is (Base)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end