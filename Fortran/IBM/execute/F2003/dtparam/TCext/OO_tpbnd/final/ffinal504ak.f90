! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal504ak
!*
!*  DATE                       : 2007-10-11 (original: 03/31/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final (STOP does not cause finalization)
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
        integer(kbase_1), pointer :: data(:) => null()

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

program ffinal504ak
    call abc
end

subroutine abc
use m
    type (base(4)) :: b1 ! tcx: (4)
    type (base(4)), allocatable :: b2 ! tcx: (4)

    allocate (b1%data(2))

    allocate (b2)
    allocate (b2%data(3))
    stop 0   !<-- there is finalization caused by this statement

    deallocate (b2)
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
