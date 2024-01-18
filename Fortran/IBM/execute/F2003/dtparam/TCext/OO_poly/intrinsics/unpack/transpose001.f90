! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/unpack/transpose001.f
! opt variations: -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/24/2005
! PRIMARY FUNCTIONS TESTED   : unpack
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(AbstractParent) :: Base    ! (4)
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program transpose001
use m
    class(AbstractParent(4)), pointer :: v1(:)
    class(*), pointer :: f1(:,:)
    logical, allocatable :: m1(:,:)

    allocate(v1(8), SOURCE=(/(Child(4)(i,i-1),i=101,108)/))
    allocate(f1(3,5), SOURCE=reshape((/(Child(4)(i,-i),i=1,15)/), (/3,5/)))
    allocate(m1(3,5), SOURCE=reshape((/.FALSE.,.TRUE.,.TRUE.,.FALSE., &
     .TRUE.,.TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE., &
     .FALSE.,.FALSE.,.TRUE./), (/3,5/)))

    select type(name1=>unpack(v1, transpose(m1), transpose(f1)))
        type is (Child(4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
