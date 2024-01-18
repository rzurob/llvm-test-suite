! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-02 (original: 03/01/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (recursive finalization for a
!                               linked-list)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type node (lnode) ! lnode=0
       integer, len :: lnode
        type (node(:)), pointer :: next => null() ! tcx: (:)

        contains

        final :: finalizeNode
    end type

    contains

    recursive subroutine finalizeNode (b)
        type (node(*)), intent(inout) :: b ! tcx: (*)

        if (associated (b%next)) then
            print *, 'deallocating next node'

            deallocate (b%next)
        end if
    end subroutine
end module

use m
    type (node(:)), allocatable :: b1 ! tcx: (:)

    allocate (node(0)::b1) ! tcx: node(0)
    allocate (node(1)::b1%next) ! tcx: node(1)
    allocate (node(2)::b1%next%next) ! tcx: node(2)
    allocate (node(3)::b1%next%next%next) ! tcx: node(3)

    deallocate (b1)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: node - added parameters (lnode) to invoke with (0) / declare with (*) - 3 changes
