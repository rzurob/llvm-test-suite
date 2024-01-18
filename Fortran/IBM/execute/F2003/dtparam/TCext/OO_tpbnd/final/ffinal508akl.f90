! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal508akl
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
    type node (knode,lnode) ! knode,lnode=4,0
       integer, kind :: knode
       integer, len :: lnode
        type (node(4,:)), pointer :: next => null() ! tcx: (4,:)

        contains

        final :: finalizeNode
    end type

    contains

    recursive subroutine finalizeNode (b)
        type (node(4,*)), intent(inout) :: b ! tcx: (4,*)

        if (associated (b%next)) then
            print *, 'deallocating next node'

            deallocate (b%next)
        end if
    end subroutine
end module

use m
    type (node(4,:)), allocatable :: b1 ! tcx: (4,:)

    allocate (node(4,0)::b1) ! tcx: node(4,0)
    allocate (node(4,1)::b1%next) ! tcx: node(4,1)
    allocate (node(4,2)::b1%next%next) ! tcx: node(4,2)
    allocate (node(4,3)::b1%next%next%next) ! tcx: node(4,3)

    deallocate (b1)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: node - added parameters (knode,lnode) to invoke with (4,0) / declare with (4,*) - 3 changes
