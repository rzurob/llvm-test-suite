! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal514bk
!*
!*  DATE                       : 2007-10-12 (original: 04/14/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (allocatable function results shall
!*                               be finalized/deallocated)
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
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: id

        contains

        procedure :: replicate => replicateBase
        final :: finalizeBase
    end type

    contains

    function replicateBase (b)
        class (base(4)), intent(in) :: b ! tcx: (4)
        type (base(4)), allocatable :: replicateBase ! tcx: (4)

        allocate (replicateBase)

        replicateBase%id = b%id
    end function

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module

use m
    type (base(4)), save :: b1, b2 ! tcx: (4)

    b1%id = 10

    print *, b1%replicate()
    b2 = b1%replicate()

    print *, b2

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
