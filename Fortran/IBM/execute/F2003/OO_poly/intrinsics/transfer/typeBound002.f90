! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeBound002.f
! %VERIFY: typeBound002.out:typeBound002.vf
! %STDIN:
! %STDOUT: typeBound002.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/20/2004
!*  ORIGIN                     :
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
    type, abstract :: AbstractParent
        contains
        procedure :: transferToMe
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    function transferToMe(this, a)
        class(AbstractParent), intent(in) :: this
        class(AbstractParent), intent(in) :: a
        class(AbstractParent), pointer :: transferToMe
        allocate(transferToMe, SOURCE=transfer(a, this))
    end function
end module

program typeBound002
use m
    class(AbstractParent), pointer :: b1
    allocate(Base::b1)

    select type (name1=>b1%transferToMe(Child(4,4)))
        type is (Base)
            print *, "Base", name1
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(Child::b1)

    select type (name1=>b1%transferToMe(Base(8)))
        type is (Child)
            print *, "Child", name1%i
        class default
            error stop 2_4
    end select
end
