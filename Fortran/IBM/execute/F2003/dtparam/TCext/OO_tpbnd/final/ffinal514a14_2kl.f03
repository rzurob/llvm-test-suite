! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-07 (original: 02/11/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of temporaries created
!                               by function call in associate construct)
!*
!*  KEYWORD(S)                 :
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

    type, extends (base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name

        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(in) :: b (:) ! tcx: (4)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,*)), intent(in) :: c ! tcx: (4,*)

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child(4,*)), intent(in) :: c (:) ! tcx: (4,*)

        print *, 'finalizeChildRank1'
    end subroutine
end module

module m1
use m
    interface makeData
        type (base(4)) function makeBaseArray (i, n) ! tcx: (4)
        use m
            dimension makeBaseArray (n)
            integer(4), intent(in) :: i, n
        end function

        type (child(4,20)) function makeChildArray (i, name, n) ! tcx: (4,20)
        use m
            dimension makeChildArray (n)
            integer(4), intent(in) :: i, n
            character(*), intent(in) :: name
        end function
    end interface
end module

program ffinal514a14_2kl
use m1
    associate (x => makeData (10, 2))
    end associate

    print *, 'test 2'

    associate (x1 => makeData (20, 'child_type_temp', 3))
    end associate

    print *, 'end'
end

type (base(4)) function makeBaseArray (i, n) ! tcx: (4)
use m
    dimension makeBaseArray (n)
    integer(4), intent(in) :: i, n

    makeBaseArray%id = i
end function


type (child(4,20)) function makeChildArray (i, name, n) ! tcx: (4,20)
use m
    dimension makeChildArray (n)
    integer(4), intent(in) :: i, n
    character(*), intent(in) :: name

    makeChildArray%id = i
    makeChildArray%name = name
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 4 changes
