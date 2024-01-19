! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/unpack/typeBound001.f
! opt variations: -qnok -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Cross testing type bound. Polymorphic.
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
        contains
        procedure :: unpackMe
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    contains

    function unpackMe(this, v1, m1)
        class(AbstractParent(4)), intent(in) :: this
        class(AbstractParent(4)), pointer :: v1(:)
        logical, intent(in) :: m1(:,:)
        class(AbstractParent(4)), pointer :: unpackMe(:,:)
        associate(name1=>unpack(v1, m1, this))
            allocate(unpackMe(size(name1,1),size(name1,2)), &
             SOURCE=name1)
        end associate
    end function
end module

program typeBound001
use m
    class(AbstractParent(4)), pointer :: b1
    class(AbstractParent(4)), pointer :: v1(:)
    logical :: m1(5,5)

    allocate(b1, SOURCE=Base(4)(2))
    allocate(v1(8), SOURCE=(/(Base(4)(i),i=1,8)/))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE., &
     .FALSE.,.TRUE./), (/5,5/), (/.FALSE.,.TRUE./))

    select type(name1=>b1%unpackMe(v1, m1(2:4,2:5)))
        type is (Base(4))
            print *, "Base", name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(b1,v1)
    allocate(b1, SOURCE=Child(4)(3,4))
    allocate(v1(8), SOURCE=(/(Child(4)(i,-i),i=101,108)/))

    select type(name1=>b1%unpackMe(v1, m1(2:,:4)))
        type is (Child(4))
            print *, "Child", name1
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
