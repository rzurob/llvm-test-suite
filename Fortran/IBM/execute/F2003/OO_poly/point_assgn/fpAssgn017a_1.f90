! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (test linked-list)
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
    type base
        integer(4) :: id

        contains

        procedure :: print => printBase
        final :: finalizeBase
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
        final :: finalizeChild
    end type

    type (base) :: b1 = base (10)
    type (child) :: c1 = child (20, 'c1')

    type (base) :: b2 (2)
    type (child) :: c2 (5)

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine
end module


module m1
use m
    type node
        type (node), pointer :: next => null()
        class (base), pointer :: data => null()
    end type

    type llist
        type (node), pointer :: head => null()

        contains

        procedure :: print => printLlist
        procedure :: push_back => addNode2End
        final :: finalizeList
    end type

    contains

    subroutine printLlist (l)
        class (llist), intent(in) :: l

        type (node), pointer :: iterator

        iterator => l%head

        do while (associated (iterator))
            if (associated (iterator%data)) then
                call iterator%data%print
            else
                print *, 'empty node'
            end if

            iterator => iterator%next
        end do
    end subroutine

    subroutine finalizeList (l)
        type (llist), intent(inout) :: l

        type (node), pointer :: tail, iterator
        integer(4) :: i = 0

        tail => l%head

        !! find the tail and deallocate the node along the way
        do while (associated (tail))
            iterator => tail

            tail => tail%next

            if (associated (iterator%data)) deallocate (iterator%data)
            deallocate (iterator)

            i = i + 1
        end do

        print *, i, 'nodes deallocated'
    end subroutine

    subroutine addNode2End (l, n)
        class (llist), intent(inout) :: l
        class (base), intent(in) :: n

        type (node), pointer :: tail

        !! treat the first node special

        if (.not. associated (l%head)) then
            allocate (l%head)

            allocate (l%head%data, source=n)
            return
        end if

        tail => l%head

        do while (associated (tail%next))
            tail => tail%next
        end do

        allocate (tail%next)
        allocate (tail%next%data, source=n)
    end subroutine
end module


program fpAssgn017a
use m
    b2%id = (/-1, -2/)

    c2%id = (/(i*100, i=1,5)/)
    c2%name = (/('c2_'//char(ichar('0')+i), i=1,5)/)

    call testLlist
end

subroutine testLlist
use m1
    type (llist) l1

    call l1%push_back (c1)

    call l1%push_back (b1)

    call l1%push_back (b2(2))

    call l1%push_back (b2(1))

    call l1%push_back (c2(3))

    call l1%push_back (c2(5))

    call l1%push_back (c2(2)%base)

    call l1%print
end subroutine
