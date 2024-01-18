! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal535k
!*
!*  DATE                       : 2007-11-12 (original: 02/06/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (final binding call for a linked
!                               list)
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
    type, private :: node (knode_1) ! knode_1=4
        integer, kind :: knode_1
        integer(knode_1) value
        type (node(knode_1)), pointer :: next => null() ! tcx: (4)
    end type

    type list (klist_1) ! klist_1=4
       integer, kind :: klist_1
        type(node(klist_1)), private, pointer :: head => null() ! tcx: (klist_1)
        type(node(klist_1)), private, pointer :: tail => null() ! tcx: (klist_1)

        contains

        final :: clearList
        procedure :: addNode => addNode2List
    end type

    contains

    subroutine addNode2List (l, value)
        class (list(4)), intent(inout) :: l ! tcx: (4)
        integer, intent(in) :: value

        if (.not. associated (l%head)) then
            allocate (l%head, source=node(4)(value)) ! tcx: (4)
            l%tail => l%head
        else
            allocate (l%tail%next, source=node(4)(value)) ! tcx: (4)
            l%tail => l%tail%next
        end if
    end subroutine

    recursive subroutine clearList (l)
        type (list(4)), intent(inout) :: l ! tcx: (4)

        type (node(4)), save, pointer :: localNode ! tcx: (4)
        if (.not. associated (l%head)) return

        if (associated(l%head, l%tail)) then
            print *, 'dellocating last node'
            deallocate (l%head)

            nullify(l%tail)
        else
            localNode => l%head%next

            print *, 'deallocating one node'
            deallocate (l%head)

            l%head => localNode

            call clearList(l)
        end if
    end subroutine
end module


program ffinal535k
use m
    type (list(4)) aList ! tcx: (4)

    call aList.addNode (10)
    call aList.addNode (100)
    call aList.addNode (1)
    call aList.addNode (-10)

    call test1 (aList)

    print *, 'end'

    contains

    subroutine test1 (l)
        class (list(4)), intent(out) :: l ! tcx: (4)
    end subroutine
end


! Extensions to introduce derived type parameters:
! type: node - added parameters (knode_1) to invoke with (4) / declare with (4) - 6 changes
! type: list - added parameters (klist_1) to invoke with (4) / declare with (4) - 4 changes
