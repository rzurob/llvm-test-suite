! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/spread/typeBound002.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/18/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Cross testing type bound.
!*    Polymorphic
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains
        procedure :: spreadMe
    end type

    type, extends(AbstractParent) :: Base(n2,k2)    ! (4,20,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
    end type

    type, extends(Base) :: Child(n3,k3)    ! (4,20,20,4,20,4)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)      j
    end type

    contains

    function spreadMe(this, i)
        class(AbstractParent(4,*)), intent(in) :: this
        integer, intent(in) :: i
        class(AbstractParent(4,:)), pointer :: spreadMe(:)
        allocate(spreadMe(i), SOURCE=spread(this, 1, i))
    end function
end module

program typeBound002
use m
    class(AbstractParent(4,:)), pointer :: b1
    allocate(b1, SOURCE=Base(4,20,20,4)(2))

    select type(name1=>b1%spreadMe(4))
        type is (Base(4,*,*,4))
            print *, "Base", name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(4,20,20,4,20,4)(3,4))

    select type(name1=>b1%spreadMe(8))
        type is (Child(4,*,*,4,*,4))
            print *, "Child", name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
