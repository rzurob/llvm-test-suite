!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/23/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : VECTOR, MASK, or FIELD is array
!                              element or array section. Poly and
!                              unlimited poly.
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
    class(*), pointer :: v1(:)
    logical :: m1(5,5)

    allocate(b1(4,5),SOURCE=reshape((/(Child(i,-i),i=1,20)/),(/4,5/)))
    allocate(v1(14),SOURCE=(/(Child(i,i+1),i=1,14)/))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE., &
     .FALSE.,.TRUE./), (/5,5/), (/.FALSE.,.TRUE./))

    select type(name1=>unpack(v1(3:12), m1(2:4,:4), b1(2:4,2:)))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
