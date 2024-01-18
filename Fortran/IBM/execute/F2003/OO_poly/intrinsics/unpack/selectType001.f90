!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/23/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : VECTOR or FIELD is an associate name of
!                              a select type construct. Associate name
!                              is poly.
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

program selectType001
use m
    class(AbstractParent), pointer :: ap1(:,:) => null()
    class(*), allocatable :: v1(:)
    logical :: m1(5,5)

    allocate(ap1(3,4),SOURCE=reshape((/(Child(i,-i),i=1,20)/),(/3,4/)))
    allocate(v1(12),SOURCE=(/(Child(i,i+1),i=11,22)/))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE., &
     .FALSE.,.TRUE./), (/5,5/), (/.FALSE.,.TRUE./))

    select type(name1=>v1(4:10))
        class is (Child)
            select type(name2=>unpack(name1,m1(2:4,2:),ap1))
                type is (Child)
                    print *, name2
                    print *, shape(name2)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select
end
