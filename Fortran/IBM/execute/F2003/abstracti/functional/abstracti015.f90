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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Alberto Alvarez-Mesquida
!*  DATE                       : 02/20/2006
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
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
    type, abstract :: AbstractParent
        contains
        procedure(myTransferInterface), deferred :: transferToMe
    end type

    abstract interface
        function myTransferInterface(this, a)
            import :: AbstractParent
            class(AbstractParent), intent(in) :: this
            class(AbstractParent), intent(in) :: a
            class(AbstractParent), pointer :: myTransferInterface
        end function
    end interface

    type, extends(AbstractParent) :: Base
        integer i
        contains
        procedure :: transferToMe => transferToBase
    end type

    type, extends(Base) :: Child
        integer j
        contains
        procedure :: transferToMe => transferToChild
    end type

    contains

    function transferToBase(this, a)
        class(Base), intent(in) :: this
        class(AbstractParent), intent(in) :: a
        class(AbstractParent), pointer :: transferToBase
        allocate(transferToBase, SOURCE=transfer(a, this))
    end function

    function transferToChild(this, a)
        class(Child), intent(in) :: this
        class(AbstractParent), intent(in) :: a
        class(AbstractParent), pointer :: transferToChild
        allocate(transferToChild, SOURCE=transfer(a, this))
    end function
end module

program abstracti015
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
