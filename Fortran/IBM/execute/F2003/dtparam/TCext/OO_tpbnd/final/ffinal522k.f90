! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-12 (original: 04/14/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (no finalization needed for pointer
!*                               function return)
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

    !! NOTE: this is written in a way intened not to get memleak using intrinsic
    !!       assignment
    function replicateBase (b)
        class (base(4)), intent(in) :: b ! tcx: (4)
        type (base(4)), pointer :: replicateBase ! tcx: (4)

        type (base(4)), target, save :: temp ! tcx: (4)

        temp%id = b%id

        replicateBase => temp
    end function

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal522k
use m
    type (base(4)), save :: b1, b2 ! tcx: (4)

    b1%id = 10

    b2 = b1%replicate()

    print *, b2, b1%replicate()
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
