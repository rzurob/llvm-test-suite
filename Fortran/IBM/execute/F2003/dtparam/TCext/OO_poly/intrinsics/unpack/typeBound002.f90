! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/unpack/typeBound002.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Cross testing type bound. Unlimited poly.
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
        contains
        procedure :: unpackMe
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type

    contains

    function unpackMe(this, v1, m1)
        class(AbstractParent(4,*)), intent(in) :: this
        class(AbstractParent(4,:)), pointer :: v1(:)
        logical, intent(in) :: m1(:,:)
        class(AbstractParent(4,:)), pointer :: unpackMe(:,:)
        associate(name1=>unpack(v1, m1, this))
            allocate(unpackMe(size(name1,1),size(name1,2)), &
             SOURCE=name1)
        end associate
    end function
end module

program typeBound002
use m
    class(*), pointer :: b1
    class(AbstractParent(4,:)), pointer :: v1(:)
    logical :: m1(5,5)

    allocate(b1, SOURCE=Base(4,20)(2))
    allocate(v1(8), SOURCE=(/(Base(4,20)(i),i=1,8)/))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE., &
     .FALSE.,.TRUE./), (/5,5/), (/.FALSE.,.TRUE./))

    select type(b1)
        class is (AbstractParent(4,*))
            select type(name1=>b1%unpackMe(v1, m1(2:4,2:5)))
                type is (Base(4,*))
                    print *, "Base", name1
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select

    deallocate(b1,v1)
    allocate(b1, SOURCE=Child(4,20)(3,4))
    allocate(v1(8), SOURCE=(/(Child(4,20)(i,-i),i=101,108)/))

    select type(b1)
        class is (AbstractParent(4,*))
            select type(name1=>b1%unpackMe(v1, m1(2:,:4)))
                type is (Child(4,*))
                    print *, "Child", name1
                    print *, shape(name1)
                class default
                    error stop 3_4
            end select
        class default
            error stop 4_4
    end select
end
