! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/typeBound002.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : Cross testing type bound. Polymorphic.
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
        procedure :: eoshiftMe
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type

    contains

    function eoshiftMe(this, i, j, b)
        class(AbstractParent(4,*)), intent(in) :: this
        integer, intent(in) :: i
        integer, intent(in) :: j
        class(AbstractParent(4,*)), intent(in) :: b
        class(AbstractParent(4,:)), pointer :: eoshiftMe(:)
        class(AbstractParent(4,:)), pointer :: temp(:)
        allocate(temp(i), SOURCE=this)
        allocate(eoshiftMe(i), SOURCE=eoshift(temp, j, b, 1))
    end function
end module

program typeBound002
use m
    class(*), pointer :: b1
    allocate(b1, SOURCE=Child(4,20)(3,4))

    select type(b1)
        type is (Child(4,*))
            select type(name1=>b1%eoshiftMe(6,-3,Child(4,20)(-3,-4)))
                type is (Child(4,*))
                    print *, "Child", name1
                    print *, size(name1)
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select
end
