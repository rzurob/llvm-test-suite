!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/24/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : MASK or FIELD is function return of
!                              transpose. Poly and unlimited poly.
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

program transpose001
use m
    class(AbstractParent), pointer :: v1(:)
    class(*), pointer :: f1(:,:)
    logical, allocatable :: m1(:,:)

    allocate(v1(8), SOURCE=(/(Child(i,i-1),i=101,108)/))
    allocate(f1(3,5), SOURCE=reshape((/(Child(i,-i),i=1,15)/), (/3,5/)))
    allocate(m1(3,5), SOURCE=reshape((/.FALSE.,.TRUE.,.TRUE.,.FALSE., &
     .TRUE.,.TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE., &
     .FALSE.,.FALSE.,.TRUE./), (/3,5/)))

    select type(name1=>unpack(v1, transpose(m1), transpose(f1)))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
