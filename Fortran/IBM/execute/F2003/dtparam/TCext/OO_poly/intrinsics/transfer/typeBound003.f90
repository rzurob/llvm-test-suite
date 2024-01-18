! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/transfer/typeBound003.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeBound003.f
! %VERIFY: typeBound003.out:typeBound003.vf
! %STDIN:
! %STDOUT: typeBound003.out
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
!*    Cross testing type bound. Deferred binding.
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
        contains
        procedure(myTransferInterface), deferred :: transferToMe
    end type

    interface
        function myTransferInterface(this, a)
            import :: AbstractParent
            class(AbstractParent(4)), intent(in) :: this
            class(AbstractParent(4)), intent(in) :: a
            class(AbstractParent(4)), pointer :: myTransferInterface
        end function
    end interface

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
        contains
        procedure :: transferToMe => transferToBase
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
        contains
        procedure :: transferToMe => transferToChild
    end type

    contains

    function transferToBase(this, a)
        class(Base(4)), intent(in) :: this
        class(AbstractParent(4)), intent(in) :: a
        class(AbstractParent(4)), pointer :: transferToBase
        allocate(transferToBase, SOURCE=transfer(a, this))
    end function

    function transferToChild(this, a)
        class(Child(4)), intent(in) :: this
        class(AbstractParent(4)), intent(in) :: a
        class(AbstractParent(4)), pointer :: transferToChild
        allocate(transferToChild, SOURCE=transfer(a, this))
    end function
end module

program typeBound003
use m
    class(AbstractParent(4)), pointer :: b1
    allocate(Base(4)::b1)

    select type (name1=>b1%transferToMe(Child(4)(4,4)))
        type is (Base(4))
            print *, "Base", name1
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(Child(4)::b1)

    select type (name1=>b1%transferToMe(Base(4)(8)))
        type is (Child(4))
            print *, "Child", name1%i
        class default
            error stop 2_4
    end select
end
