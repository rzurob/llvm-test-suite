! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/06/2005
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
    type, private :: node
        integer value
        type (node), pointer :: next => null()
    end type

    type list
        type(node), private, pointer :: head => null()
        type(node), private, pointer :: tail => null()

        contains

        final :: clearList
        procedure :: addNode => addNode2List
    end type

    contains

    subroutine addNode2List (l, value)
        class (list), intent(inout) :: l
        integer, intent(in) :: value

        if (.not. associated (l%head)) then
            allocate (l%head, source=node(value))
            l%tail => l%head
        else
            allocate (l%tail%next, source=node(value))
            l%tail => l%tail%next
        end if
    end subroutine

    recursive subroutine clearList (l)
        type (list), intent(inout) :: l

        type (node), save, pointer :: localNode
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


program ffinal535
use m
    type (list) aList

    call aList.addNode (10)
    call aList.addNode (100)
    call aList.addNode (1)
    call aList.addNode (-10)

    call test1 (aList)

    print *, 'end'

    contains

    subroutine test1 (l)
        class (list), intent(out) :: l
    end subroutine
end
