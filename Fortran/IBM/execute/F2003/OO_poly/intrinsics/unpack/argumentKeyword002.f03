!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/23/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Actual arguments are speficied using
!                              argument keywords. Unlimited poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer :: i = 8
    end type

    type, extends(Base) :: Child
        integer :: j = 9
    end type
end module

program argumentKeyword002
use m
    class(*), pointer :: v1(:)
    class(*), pointer :: b1(:,:,:)
    logical, pointer :: m1(:,:,:)
    allocate(b1(3,2,2), SOURCE=reshape((/(Child(i,-i),i=1,12)/), &
     (/3,2,2/)))
    allocate(v1(8), SOURCE=(/(Child(i,-i),i=101,108)/))
    allocate(m1(3,2,2), SOURCE=reshape((/.FALSE.,.TRUE.,.TRUE., &
     .FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE.,.TRUE., &
     .FALSE./), (/3,2,2/)))

    select type(name1=>unpack(MASK=m1,FIELD=b1,VECTOR=v1))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end