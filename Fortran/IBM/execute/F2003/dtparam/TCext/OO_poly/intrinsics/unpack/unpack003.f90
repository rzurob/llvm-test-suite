! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/unpack/unpack003.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/22/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : VECTOR is poly. FIELD is scalar.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type
end module

program unpack003
use m
    class(Base(4,:)), pointer :: b1(:)
    logical :: m1(3,4)
    class(Base(4,:)), allocatable :: f1

    allocate(b1(6), SOURCE=(/(Child(4,20)(i,-i),i=1,6)/))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., &
     .FALSE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./), (/3,4/))
    allocate(f1, SOURCE=Child(4,20)(9,-9))

    select type(name1=>unpack(b1, m1, f1))
        type is (Child(4,*))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
