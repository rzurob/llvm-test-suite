! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal508l
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
    type base (lbase1) ! lbase1=0
       integer, len :: lbase1
        class (base(:)), pointer :: data => null() ! tcx: (:)

        contains

        final :: finalizeBase
    end type

    private finalizeBase

    contains

    recursive subroutine finalizeBase (b)
        type (base(*)), intent(inout) :: b ! tcx: (*)

        print *, 'finalizeBase'
        if (associated (b%data)) then
            print *, 'deallocating data'
            deallocate (b%data)
            print *, 'data deallocated'
        end if
    end subroutine
end module

program ffinal508l
use m
    type (base(:)), pointer :: b1 ! tcx: (:)

    allocate(base(0)::b1) ! tcx: base(0)
    allocate(base(0)::b1%data) ! tcx: base(0)
    allocate (base(0)::b1%data%data) ! tcx: base(0)

    deallocate(b1)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase1) to invoke with (0) / declare with (*) - 3 changes
