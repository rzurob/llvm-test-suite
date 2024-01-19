! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-11 (original: 03/31/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final (module entities are not finalized when
!*                               executing END PROGRAM stmt; use m1 in
!*                               subroutine)
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
        integer(kbase_1), pointer :: data(:)

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase(b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'

        if (associated (b%data)) deallocate (b%data)
    end subroutine
end module

module m1
use m
    type (base(4)) b1_m ! tcx: (4)
    type (base(4)), allocatable :: b2_m ! tcx: (4)
end module

program ffinal503ak
    call abc

    print *, 'end'
end

subroutine abc
use m1
    allocate (b1_m%data(2))

    allocate(b2_m)
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
