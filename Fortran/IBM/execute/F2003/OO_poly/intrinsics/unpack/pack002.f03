!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Transformation back and forth using pack
!                              and unpack. Unlimited poly.
!                              If
!                                pack(a,m,v)=b
!                              Then
!                                unpack(b,m,unpack(pack(a,^m),^m,v))=a
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

program pack002
use m
    class(*), pointer :: a1(:,:)
    class(*), allocatable :: v1(:)
    logical :: m1(4,3)

    allocate(a1(4,3), SOURCE=reshape((/(Child(-i,i),i=1,12)/),(/4,3/)))
    allocate(v1(8), SOURCE=(/(Child(i,-i),i=101,108)/))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE./), (/4,3/), (/.FALSE.,.TRUE./))

    associate(name1=>pack(a1, m1, v1))
        select type(name2=>unpack(name1, m1, &
         unpack(pack(a1,(.NOT. m1)), (.NOT. m1), Child(1,1))))
            type is (Child)
                print *, name2
                print *, shape(name2)
            class default
                error stop 1_4
        end select
    end associate
end