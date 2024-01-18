! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass013.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/06/2005
!*
!*  DESCRIPTION                : class keyword (defined assignment for
!                               linked-list)
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
    type, private :: node(k1,n1)    ! (4,20)
        integer, kind             :: k1
        integer, len              :: n1
        class(*), allocatable :: value
        type(node(k1,:)), pointer :: next => null()
    end type

    type list(k2)    ! (4)
        integer, kind                      :: k2
        type(node(k2,:)), private, pointer :: head => null()
        type(node(k2,:)), private, pointer :: tail => null()

        contains

        final :: clearList
        procedure :: addNode => addNode2List
        procedure :: print => printList
    end type

    interface assignment(=)
        module procedure l1CopyL2
    end interface

    contains

    subroutine addNode2List (l, value)
        class (list(4)), intent(inout) :: l
        class(*), intent(in) :: value

        if (.not. associated (l%head)) then
            allocate (l%head, source=node(4,20)(value))
            l%tail => l%head
        else
            allocate (l%tail%next, source=node(4,20)(value))
            l%tail => l%tail%next
        end if
    end subroutine

    recursive subroutine clearList (l)
        type (list(4)), intent(inout) :: l

        type (node(4,:)), save, pointer :: localNode
        if (.not. associated (l%head)) return

        if (associated(l%head, l%tail)) then
!            print *, 'dellocating last node'
            deallocate (l%head)

            nullify(l%tail)
        else
            localNode => l%head%next

!            print *, 'deallocating one node'
            deallocate (l%head)

            l%head => localNode

            call clearList(l)
        end if
    end subroutine

    subroutine printList (l)
        class (list(4)), intent(in) :: l

        type (node(4,:)), pointer :: localNode

        localNode => l%head

        do while (associated (localNode))
            select type (y => localNode%value)
                type is (integer)
                    write (*, *) y
                type is (real)
                    write (*, '(f12.2)') y
                type is (character(*))
                    write (*, *) y
                class default
                    write (*, *) 'other data type'
            end select

            localNode => localNode%next

        end do
    end subroutine

    subroutine l1CopyL2 (l1, l2)
        class (list(4)), intent(out) :: l1
        class (list(4)), intent(in) :: l2

        type (node(4,:)), pointer :: iterator

        iterator => l2%head

        do while (associated (iterator))
            call l1%addNode (iterator%value)

            iterator => iterator%next
        end do
    end subroutine
end module


program fclass013
use m
    type (list(4)) aList, bList

    call aList%addNode (10)
    call aList%addNode ('xlftest')
    call aList%addNode (1.5)
    call aList%addNode ((-10 > 1))

    !! test the assignment
    bList = aList

    call clearList (aList)

    print *, 'test 1'

    call aList%print
    write (*, '(/,1x, a,/)') 'test 2'

    call bList%print

    !! test the assignment again
    write (*, '(/,1x, a,/)') 'test 3'

    call aList%addNode ('team')
    call aList%addNode ('work')
    call aList%addNode (10.4)
    call aList%addNode (-100)

    bList = aList

    call clearList (aList)

    call bList%print

end
