! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-02 (original: 04/15/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of the temps created by
!*                               the function result in PRINT statement;
!*                               function return arrays)
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
        integer(kbase_1) :: id

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    private finalizeBase, finalizeBaseRank1

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent (in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(in) :: b(:) ! tcx: (4)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

module m1
use m
    type, extends (base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name

        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    subroutine finalizeChild (c)
        type (child(4,*)), intent (in) :: c ! tcx: (4,*)

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child(4,*)), intent (in) :: c(:) ! tcx: (4,*)

        print *, 'finalizeChildRank1'
    end subroutine
end module

program ffinal514a10kl
use m1

    interface produceObj
        function produceBase (i, n)
        use m
            type (base(4)) produceBase(n) ! tcx: (4)
            integer*4, intent(in) :: i, n
        end function

        function produceChildObj (i, c, n)
        use m1
            type (child(4,20)) produceChildObj(n) ! tcx: (4,20)
            integer*4, intent(in) :: i, n
            character(*), intent(in) :: c
        end function
    end interface

    print *, produceObj (10, 2)
    print *, produceObj (10, 'c1', 2)
    print *, 'end'
end

function produceBase (i, n)
use m
    type (base(4)) produceBase(n) ! tcx: (4)
    integer*4, intent(in) :: i, n

    produceBase%id = i
end function

function produceChildObj (i, c, n)
use m1, only : child
    type (child(4,20)) produceChildObj(n) ! tcx: (4,20)
    integer*4, intent(in) :: i
    character(*), intent(in) :: c

    produceChildObj%id = i
    produceChildObj%name = c
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 4 changes
