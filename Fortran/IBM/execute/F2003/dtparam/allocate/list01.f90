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
    logical(4), external :: precision_r4
    type(node(:)), pointer :: list, iterator

    allocate (node(1) :: list)

    list%data%data = sin(.1)

    iterator => list

    !! insert 500 nodes
    do i = 1, 500
        allocate (node(i+1) :: iterator%next)

        iterator => iterator%next
        iterator%data%data = sin ([(j*1.0e-1, j=1,2*i+1)])
    end do

    !! validate all the nodes

    iterator => list
    do i = 0, 500
        do j = 1, 2*i + 1
            if (.not. precision_r4(iterator%data%data(j), sin(j*1.0e-1))) stop 10
        end do

        iterator => iterator%next
    end do
end
