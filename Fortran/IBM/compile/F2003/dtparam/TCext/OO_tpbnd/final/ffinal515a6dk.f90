! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal515a6dk
!*
!*  DATE                       : 2007-11-11 (original: 02/14/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (C738, finalization referenced in
!                               FORALL body must be pure)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: id

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal515a6dk
use m
    type (base(4)), allocatable :: b1(:) ! tcx: (4)

    allocate (b1(10))

    b1%id = (/(i,i=1,10)/)

    forall (i=1:10)
        b1(i) = base(4)(-i)            !<-- illegal ! tcx: (4)
    end forall
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
