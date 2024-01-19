!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/22/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : VECTOR is unlimited poly. FIELD is array.
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

program unpack006
use m
    class(*), pointer :: b1(:)
    logical :: m1(3,4)
    class(*), allocatable :: f1(:,:)

    allocate(b1(6), SOURCE=(/(Child(i,-i),i=1,6)/))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., &
     .FALSE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./), (/3,4/))
    allocate(f1(3,4), SOURCE=reshape((/(Child(i,-i),i=101,112)/), &
     (/3,4/),ORDER=(/2,1/)))

    select type(name1=>unpack(b1, m1, f1))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
