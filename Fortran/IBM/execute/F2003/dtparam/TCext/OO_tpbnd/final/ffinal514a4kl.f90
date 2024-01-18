! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-12 (original: 04/13/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of temporary created by
!*                               function call in call statement)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) id

        contains

        procedure :: print => printBase

        final :: finalizeBase
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name

        contains

        procedure :: print => printChild

        procedure :: makeObj => replicateChildObj

        final :: finalizeChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b ! tcx: (4)

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b ! tcx: (4,*)

        print *, b%id, b%name
    end subroutine

    type (child(4,20)) function replicateChildObj (c) ! tcx: (4,20)
        class (child(4,*)), intent(in) :: c ! tcx: (4,*)

        replicateChildObj%id = c%id
        replicateChildObj%name = c%name
    end function

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,*)), intent(in) :: c ! tcx: (4,*)

        print *, 'finalizeChild'
    end subroutine
end module

program ffinal514a4kl
use m
    interface
        subroutine printVal (b)
        use m
            class (base(4)), intent(in) :: b ! tcx: (4)
        end subroutine
    end interface

    type (child(4,20)) :: c1 = child(4,20) (10, 'c1_static') ! tcx: (4,20) ! tcx: (4,20)

    call printVal (c1%makeObj())

end


subroutine printVal (b)
use m
    class (base(4)), intent(in) :: b ! tcx: (4)

    call b%print
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 6 changes
