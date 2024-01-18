!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! PROGRAMMER                 : Yong Du
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Cross testing type bound. Polymorphic.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent
        contains
        procedure :: eoshiftMe
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    function eoshiftMe(this, i, j, b)
        class(AbstractParent), intent(in) :: this
        integer, intent(in) :: i
        integer, intent(in) :: j
        class(AbstractParent), intent(in) :: b
        class(AbstractParent), pointer :: eoshiftMe(:)
        class(AbstractParent), pointer :: temp(:)
        allocate(temp(i), SOURCE=this)
        allocate(eoshiftMe(i), SOURCE=eoshift(temp, j, b))
    end function
end module

program typeBound001
use m
    class(AbstractParent), pointer :: b1
    allocate(b1, SOURCE=Base(2))

    select type(name1=>b1%eoshiftMe(4,-1,Base(-2)))
        type is (Base)
            print *, "Base", name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(3,4))

    select type(name1=>b1%eoshiftMe(8,3,Child(-3,-4)))
        type is (Child)
            print *, "Child", name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
