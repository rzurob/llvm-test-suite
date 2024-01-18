!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/24/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : VECTOR or FIELD is function return of
!                              transfer. Poly and unlimited poly.
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

program transfer003
use m
    class(AbstractParent), pointer :: v1(:)
    class(AbstractParent), allocatable :: f1(:,:,:)
    class(*), pointer :: v2(:)
    class(*), allocatable :: f2(:,:)
    logical :: m1(6)

    allocate(v1(12), SOURCE=(/(Base(i),i=1,12)/))
    allocate(f1(2,2,2), SOURCE=reshape((/(Child(i,i-1),i=101,108)/), &
     (/2,2,2/)))
    allocate(v2(10), SOURCE=(/(Child(i,-i),i=1,10)/))
    allocate(f2(3,4), SOURCE=reshape((/(Base(i),i=101,112)/),(/3,4/)))
    m1 = (/.FALSE.,.TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE./)

    select type(name1=>unpack(transfer(v1, v2), m1, transfer(f2, f1)))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>unpack(transfer(v2, v1), &
     reshape(m1,(/16/),(/.TRUE.,.FALSE./)), transfer(f1, f2)))
        type is (Base)
            print *, name1
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
