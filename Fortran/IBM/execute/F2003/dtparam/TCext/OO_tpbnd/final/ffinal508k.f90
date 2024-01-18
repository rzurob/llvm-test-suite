! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal508k
!*
!*  DATE                       : 2007-10-11 (original: 04/01/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (linked list finalization may be
!*                               recursive)
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
    type base (kbase1) ! kbase1=1
       integer, kind :: kbase1
        class (base(kbase1)), pointer :: data => null() ! tcx: (1)

        contains

        final :: finalizeBase
    end type

    private finalizeBase

    contains

    recursive subroutine finalizeBase (b)
        type (base(1)), intent(inout) :: b ! tcx: (1)

        print *, 'finalizeBase'
        if (associated (b%data)) then
            print *, 'deallocating data'
            deallocate (b%data)
            print *, 'data deallocated'
        end if
    end subroutine
end module

program ffinal508k
use m
    type (base(1)), pointer :: b1 ! tcx: (1)

    allocate(b1)
    allocate(b1%data)
    allocate (b1%data%data)

    deallocate(b1)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase1) to invoke with (1) / declare with (1) - 3 changes
