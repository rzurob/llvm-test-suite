!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 11/28/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Statements: An extended type includes all of the
!                               type parameters of its parent.
!                               Case: a linked list using the extended type as
!                               the nodes.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, l)
        integer, kind :: k
        integer, len :: l

        integer(k) data(l)
    end type

    type, extends(base) :: listNode
        type(listNode(k, l)), pointer :: next => null()
    end type

    type(listNode(4, 3)), target, save:: list1
    type(listNode(l=10, k=8)), target, save:: list2

    contains

    subroutine printLists
        type(listNode(4,3)), pointer :: iterator1
        type(listNode(l=10, k=8)), pointer :: iterator2

        iterator1 => list1
        iterator2 => list2

        print *, 'list 1'

        do while (associated(iterator1))
            print *, iterator1%data

            iterator1 => iterator1%next
        end do

        print *, ''
        print *, 'list 2'

        do while (associated(iterator2))
            print *, iterator2%data

            iterator2 => iterator2%next
        end do
    end subroutine
end module

program dtparamExtends014
use m
    external setupLists

    call setupLists

    call printlists
end

subroutine setupLists
use m
    call setupList1
    call setupList2

    contains

    !! add two nodes to the list
    subroutine setupList1
        list1%data = (/1, 2, 3/)

        allocate (list1%next)
        list1%next%data = (/4,5,6/)

        allocate (list1%next%next)
        list1%next%next%data = (/7,8,9/)
    end subroutine

    !! add one node to the list
    subroutine setupList2
        list2%data = (/(i, i=1, 10)/)

        allocate(list2%next)
        list2%next%data = (/(i*10, i=1, 10)/)
    end subroutine
end
