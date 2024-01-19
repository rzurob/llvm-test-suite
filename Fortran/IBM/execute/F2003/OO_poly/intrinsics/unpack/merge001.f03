!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/24/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : VECTOR, MASK, or FIELD is function return
!                              of merge. Poly and unlimited poly.
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

program merge001
use m
    class(AbstractParent), allocatable :: v1(:)
    class(AbstractParent), pointer :: c1(:,:)
    class(*), pointer :: b1(:,:)
    logical :: m1(4,2)

    allocate(v1(8), SOURCE=(/(Child(i,-i),i=101,108)/))
    allocate(c1(4,2), SOURCE=reshape((/(Child(i,-i),i=1,8)/),(/4,2/)))
    allocate(b1(4,2), SOURCE=reshape((/(Child(-i,i+1),i=1,8)/),(/4,2/)))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.FALSE.,.TRUE., &
     .TRUE.,.FALSE./), (/4,2/))

    select type(name1=>unpack(v1, m1, merge(c1,b1,m1)))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
