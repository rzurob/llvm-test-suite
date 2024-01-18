!**********************************************************************
!* ==================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!* ==================================================================== 
!*  TEST CASE NAME             : abstracti015lk
!*  PROGRAMMER                 : Glen Mateer (derived from abstracti015
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-10-25 (original: 02/20/2006)
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters transfer
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*  DRIVER STANZA              : xlf2003 (original: xlf90)
!*
!*  DESCRIPTION                :
!*  Cross testing type bound. Deferred binding.
!*  Polymorphic
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ====================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!* ====================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type, abstract :: AbstractParent (lAbstractParent_1) ! lAbstractParent_1=7
       integer, len :: lAbstractParent_1
        contains
        procedure(myTransferInterface), deferred :: transferToMe
    end type

    abstract interface
        function myTransferInterface(this, a)
            import :: AbstractParent
            class(AbstractParent(*)), intent(in) :: this ! tcx: (*)
            class(AbstractParent(*)), intent(in) :: a ! tcx: (*)
            class(AbstractParent(:)), pointer :: myTransferInterface ! tcx: (:)
        end function
    end interface

    type, extends(AbstractParent) :: Base
        integer i
        contains
        procedure :: transferToMe => transferToBase
    end type

    type, extends(Base) :: Child (kChild_1) ! kChild_1=4
       integer, kind :: kChild_1
        integer(kChild_1) j
        contains
        procedure :: transferToMe => transferToChild
    end type

    contains

    function transferToBase(this, a)
        class(Base(*)), intent(in) :: this ! tcx: (*)
        class(AbstractParent(*)), intent(in) :: a ! tcx: (*)
        class(AbstractParent(:)), pointer :: transferToBase ! tcx: (:)
        allocate(transferToBase, SOURCE=transfer(a, this))
    end function

    function transferToChild(this, a)
        class(Child(*,4)), intent(in) :: this ! tcx: (*,4)
        class(AbstractParent(*)), intent(in) :: a ! tcx: (*)
        class(AbstractParent(:)), pointer :: transferToChild ! tcx: (:)
        allocate(transferToChild, SOURCE=transfer(a, this))
    end function
end module

program abstracti015lk
use m
    class(AbstractParent(:)), pointer :: b1 ! tcx: (:)
    allocate(Base(7)::b1) ! tcx: (7)

    select type (name1=>b1%transferToMe(Child(7,4)(4,4))) ! tcx: (7,4)
        type is (Base(*)) ! tcx: (*)
            print *, "Base", name1
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(Child(7,4)::b1) ! tcx: (7,4)

    select type (name1=>b1%transferToMe(Base(7)(8))) ! tcx: (7)
        type is (Child(*,4)) ! tcx: (*,4)
            print *, "Child", name1%i
        class default
            error stop 2_4
    end select
end


! Extensions to introduce derived type parameters:
! type: AbstractParent - added parameters (lAbstractParent_1) to invoke with (7) / declare with (*) - 8 changes
! type: Child - added parameters (kChild_1) to invoke with (*,4) / declare with (7,4) - 4 changes
