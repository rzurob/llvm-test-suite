! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-31 (original: 06/21/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (deallocte of an unlimited
!                               poly-pointer)
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

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal014k
use m
    type (base(4)), pointer :: b_ptr ! tcx: (4)

    class (*), pointer :: x

    allocate (b_ptr)

    x => b_ptr

    deallocate (x)
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
