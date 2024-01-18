!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2008/10/14
!*
!*  DESCRIPTION                : test ALLOCATE statement, apply type-spec on a
!linked list with deferred length type parameter.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real :: data (n*2+1)
    end type

    type node (n)
        integer, len :: n

        type(base(n)) data
        type(node(:)), pointer :: next => null()

        contains

        procedure :: print => printNode
    end type

    contains

    recursive subroutine printNode (n)
        class(node(*)), intent(in) :: n

        print *, n%data
        if (associated(n%next)) call n%next%print
    end subroutine
end module

use m
    implicit none
    integer i, j
    type(node(:)), pointer :: list, iterator

    allocate (node(1) :: iterator)

    iterator%data%data = sin(.1)

    list => iterator

    !! insert 50 nodes
    do i = 1, 50
        allocate (node(i+1) :: iterator%next)

        iterator => iterator%next
        iterator%data%data = sin ([(j*1.0e-1, j=1,2*i+3)])
    end do

    !! validate all the nodes

    call list%print
end
