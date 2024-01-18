! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal013kk
!*
!*  DATE                       : 2007-10-31 (original: 02/10/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of a linked-list)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: id

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase', b%id
    end subroutine
end module

module m
    type container (kC) ! kC=1
       integer, kind :: kC
        type (container(kC)), pointer :: next => null() ! tcx: (kC)
        class (*), pointer :: data => null()

        contains

        FINAL :: finalizeContainer
    end type

    contains

    subroutine finalizeContainer (c)
        type (container(1)), intent(inout), target :: c ! tcx: (1)

        type (container(1)), pointer :: iterator ! tcx: (1)

        iterator => c

        do while (associated(iterator))
            if (.not. associated(iterator%data)) exit

            deallocate (iterator%data)
            iterator => iterator%next
        end do
    end subroutine
end module

program ffinal013kk
use m
use m1

    type (base(4)), pointer :: b_ptr1, b_ptr2, b_ptr3 ! tcx: (4)

    type (container(1)), allocatable :: list ! tcx: (1)
    type (container(1)), target :: node2, node3 ! tcx: (1)

    allocate (b_ptr1, b_ptr2, b_ptr3)
    allocate (list)

    b_ptr1%id = 1
    b_ptr2%id = 2
    b_ptr3%id = 3

    node3%data => b_ptr3

    node2%data => b_ptr2
    node2%next => node3


    list%data  => b_ptr1
    list%next  => node2

    !! now invoke the container's finalizer
    print *, 'test'

    deallocate (list)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: container - added parameters (kC) to invoke with (1) / declare with (1) - 5 changes
