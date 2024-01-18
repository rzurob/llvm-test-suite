! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/typeBound001.f
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
    type, abstract :: AbstractParent(n1)    ! (20)
        integer, len :: n1
        contains
        procedure :: eoshiftMe
    end type

    type, extends(AbstractParent) :: Base(k1)    ! (20,4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2)    ! (20,4,4)
        integer, kind :: k2
        integer(k2)      j
    end type

    contains

    function eoshiftMe(this, i, j, b)
        class(AbstractParent(*)), intent(in) :: this
        integer, intent(in) :: i
        integer, intent(in) :: j
        class(AbstractParent(*)), intent(in) :: b
        class(AbstractParent(:)), pointer :: eoshiftMe(:)
        class(AbstractParent(:)), pointer :: temp(:)
        allocate(temp(i), SOURCE=this)
        allocate(eoshiftMe(i), SOURCE=eoshift(temp, j, b))
    end function
end module

program typeBound001
use m
    class(AbstractParent(:)), pointer :: b1
    allocate(b1, SOURCE=Base(20,4)(2))

    select type(name1=>b1%eoshiftMe(4,-1,Base(20,4)(-2)))
        type is (Base(*,4))
            print *, "Base", name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(20,4,4)(3,4))

    select type(name1=>b1%eoshiftMe(8,3,Child(20,4,4)(-3,-4)))
        type is (Child(*,4,4))
            print *, "Child", name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
