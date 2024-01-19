!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/24/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : VECTOR or FIELD is function return of
!                              reshape. Poly and unlimited poly.
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

program reshape003
use m
    class(*), pointer :: v1(:,:)
    class(AbstractParent), pointer :: f1(:)
    logical :: m1(12)

    allocate(v1(2,4),SOURCE=reshape((/(Child(i,-i),i=11,18)/),(/2,4/)))
    allocate(f1(15), SOURCE=(/(Child(i,-i),i=101,115)/))
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., &
     .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE./)

    select type(name1=>unpack(reshape(v1,(/8/)), reshape(m1, (/3,4/)), &
     reshape(f1,(/3,4/),(/Child(-1,-2), Child(-3,-4)/),(/2,1/))))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
