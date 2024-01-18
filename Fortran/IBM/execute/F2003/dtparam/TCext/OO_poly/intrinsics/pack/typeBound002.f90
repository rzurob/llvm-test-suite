! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/pack/typeBound002.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : Cross testing type bound. Unlimited
!                              polymorphic.
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
        procedure :: packMe
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type

    contains

    function packMe(this, i, m1)
        class(AbstractParent(4,*)), intent(in) :: this
        integer, intent(in) :: i
        logical, intent(in) :: m1(i,i)
        class(AbstractParent(4,:)), pointer :: packMe(:)
        class(AbstractParent(4,:)), pointer :: temp(:,:)
        allocate(temp(i,i), SOURCE=this)
        select type(name1=>pack(temp, m1))
            type is (Base(4,*))
                allocate(packMe(size(name1)), SOURCE=name1)
            type is (Child(4,*))
                allocate(packMe(size(name1)), SOURCE=name1)
            class default
                error stop 3_4
        end select
    end function
end module

program typeBound002
use m
    class(*), pointer :: b1
    logical :: m1(5,5)

    allocate(b1, SOURCE=Child(4,20)(3,4))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE., &
     .FALSE.,.TRUE./), (/5,5/), (/.FALSE.,.TRUE./))

    select type(b1)
        class is (AbstractParent(4,*))
            select type(name1=>b1%packMe(4,m1(2:,2:)))
                type is (Child(4,*))
                    print *, "Child", name1
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select
end
