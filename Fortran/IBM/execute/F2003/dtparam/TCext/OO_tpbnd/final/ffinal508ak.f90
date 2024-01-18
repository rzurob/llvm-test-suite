! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal508ak
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal508a by Jim Xia)
!*  DATE                       : 2007-11-02 (original: 03/01/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final sub (recursive finalization for a
!                               linked-list)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type node (knode) ! knode=4
       integer, kind :: knode
        type (node(knode)), pointer :: next => null() ! tcx: (4)

        contains

        final :: finalizeNode
    end type

    contains

    recursive subroutine finalizeNode (b)
        type (node(4)), intent(inout) :: b ! tcx: (4)

        if (associated (b%next)) then
            print *, 'deallocating next node'

            deallocate (b%next)
        end if
    end subroutine
end module

use m
    type (node(4)), allocatable :: b1 ! tcx: (4)

    allocate (b1)
    allocate (b1%next)
    allocate (b1%next%next)
    allocate (b1%next%next%next)

    deallocate (b1)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: node - added parameters (knode) to invoke with (4) / declare with (4) - 3 changes
