! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/unpack/pack001.f
! opt variations: -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Transformation back and forth using pack
!                              and unpack. Poly.
!                              If
!                                pack(a,m,v)=b
!                              Then
!                                unpack(pack(a,^m),^m,unpack(b,m,v))=a
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

program pack001
use m
    class(AbstractParent(4)), pointer :: a1(:,:)
    class(AbstractParent(4)), allocatable :: v1(:)
    logical :: m1(4,3)

    allocate(a1(4,3), SOURCE=reshape((/(Child(4)(-i,i),i=1,12)/),(/4,3/)))
    allocate(v1(8), SOURCE=(/(Child(4)(i,-i),i=101,108)/))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE./), (/4,3/), (/.FALSE.,.TRUE./))

    associate(name1=>pack(a1, m1, v1))
        select type(name2=>unpack(pack(a1,(.NOT. m1)), (.NOT. m1), &
         unpack(name1, m1, Child(4)(1,1))))
            type is (Child(4))
                print *, name2
                print *, shape(name2)
            class default
                error stop 1_4
        end select
    end associate
end
