! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal514a15kl
!*
!*  DATE                       : 2007-11-07 (original: 04/20/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of temps created by
!*                               function calls in CASE construct)
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

        final :: finalizeBase
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name

        contains

        final :: finalizeChild
    end type

    interface operator (==)
        logical function baseEqual (b1, b2)
        import base
            type (base(4)), intent(in) :: b1, b2 ! tcx: (4)
        end function

        logical function childEqual (c1, c2)
        import child
            type (child(4,*)), intent(in) :: c1, c2 ! tcx: (4,*)
        end function
    end interface

    interface makeData
        function makeBaseObj (i)
        import base
            type (base(4)) :: makeBaseObj ! tcx: (4)
            integer*4, intent(in) :: i
        end function

        function makeChildObj (i, c)
        import child
            type (child(4,20)) :: makeChildObj ! tcx: (4,20)
            integer*4, intent(in) :: i
            character(*), intent(in) :: c
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,*)), intent(in) :: c ! tcx: (4,*)

        print *, 'finalizeChild'
    end subroutine
end module

logical function baseEqual (b1, b2)
use m, only: base
    type (base(4)), intent(in) :: b1, b2 ! tcx: (4)

    baseEqual = (b1%id == b2%id)
end function

logical function childEqual (c1, c2)
use m, only: child, operator(==), base
    type (child(4,*)), intent(in) :: c1, c2 ! tcx: (4,*)

    childEqual = ((c1%base == c2%base) .and. (c1%name == c2%name))
end function

function makeBaseObj (i)
use m, only: base
    type (base(4)) :: makeBaseObj ! tcx: (4)
    integer*4, intent(in) :: i

    makeBaseObj%id = i
end function

function makeChildObj (i, c)
use m, only:child
    type (child(4,20)) :: makeChildObj ! tcx: (4,20)
    integer*4, intent(in) :: i
    character(*), intent(in) :: c

    makeChildObj%id = i
    makeChildObj%name = c
end function


program ffinal514a15kl
use m
    type (child(4,20)) :: c1 = child(4,20) (10, 'c1_static') ! tcx: (4,20) ! tcx: (4,20)
    type (base(4)) :: b1 = base(4) (100) ! tcx: (4) ! tcx: (4)

    select case (c1 == makeData (10, 'temp'))
        case (.true.)
            error stop 101_4
        case (.false.)
            print *, 'success'
    end select

    select case (b1 == makeData (100))
        case (.true.)
            print *, 'success'
        case (.false.)
            error stop 2_4
    end select

    print *, (base(4)(10) == makeData (10)) ! tcx: (4)
    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 8 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 7 changes
