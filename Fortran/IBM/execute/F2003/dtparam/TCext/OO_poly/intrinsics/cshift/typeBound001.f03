! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/typeBound001.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 02/02/2005
! PRIMARY FUNCTIONS TESTED   : cshift
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
        procedure :: cshiftMe
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type

    contains

    function cshiftMe(this, i, j)
        class(AbstractParent(4,*)), intent(in) :: this
        integer, intent(in) :: i
        integer, intent(in) :: j
        class(AbstractParent(4,:)), pointer :: cshiftMe(:)
        class(AbstractParent(4,:)), pointer :: temp(:)
        allocate(temp(i), SOURCE=this)
        allocate(cshiftMe(i), SOURCE=cshift(temp, j))
    end function
end module

program typeBound001
use m
    class(AbstractParent(4,:)), pointer :: b1
    allocate(b1, SOURCE=Base(4,20)(2))

    select type(name1=>b1%cshiftMe(4,-3))
        type is (Base(4,*))
            print *, "Base", name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(4,20)(3,4))

    select type(name1=>b1%cshiftMe(8,5))
        type is (Child(4,*))
            print *, "Child", name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
