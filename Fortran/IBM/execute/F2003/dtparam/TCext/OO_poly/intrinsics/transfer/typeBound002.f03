! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/transfer/typeBound002.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/20/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
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
        procedure :: transferToMe
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type

    contains

    function transferToMe(this, a)
        class(AbstractParent(4,*)), intent(in) :: this
        class(AbstractParent(4,*)), intent(in) :: a
        class(AbstractParent(4,:)), pointer :: transferToMe
        allocate(transferToMe, SOURCE=transfer(a, this))
    end function
end module

program typeBound002
use m
    class(AbstractParent(4,:)), pointer :: b1
    allocate(Base(4,20)::b1)

    select type (name1=>b1%transferToMe(Child(4,20)(4,4)))
        type is (Base(4,*))
            print *, "Base", name1
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(Child(4,20)::b1)

    select type (name1=>b1%transferToMe(Base(4,20)(8)))
        type is (Child(4,*))
            print *, "Child", name1%i
        class default
            error stop 2_4
    end select
end
