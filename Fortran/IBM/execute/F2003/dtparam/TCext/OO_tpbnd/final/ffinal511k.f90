! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-02 (original: 04/11/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (for VALUE attribute, there is no
!                               finalizations even though an copy is used)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1), pointer :: data => null()

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'
        if (associated (b%data)) deallocate (b%data)
    end subroutine
end module

program ffinal511k
use m
    class (base(4)), allocatable :: b1 ! tcx: (4)

    allocate (b1)

    allocate (b1%data, source=100)

    print *, 'begin'

    call test1 (b1)

    print *, 'end'

    contains

    subroutine test1 (b)
        type (base(4)), value :: b ! tcx: (4)

        print *, associated (b%data)
    end subroutine
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
