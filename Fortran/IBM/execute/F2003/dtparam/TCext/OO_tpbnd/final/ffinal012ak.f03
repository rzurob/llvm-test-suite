! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-31 (original: 04/26/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (no-autodeallocation of allocated
!*                               allocatables in the main program)
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
        integer(kbase_1) :: x

        contains

        final :: finalizeBase
        final :: finalizeBaseArray
    end type

    type p (kp_1) ! kp_1=4
       integer, kind :: kp_1
        type (base(kp_1)), allocatable :: b(:) ! tcx: (kp_1)
    end type

    contains
    subroutine finalizeBase (b1)
        type (base(4)), intent(inout) :: b1 ! tcx: (4)
        print *, 'in finalizeBase'
    end subroutine

    subroutine finalizeBaseArray (b1)
        type (base(4)), intent(in) :: b1(:) ! tcx: (4)
        print *, 'in finalizeBaseArray'
    end subroutine

end module


program ffinal012ak
use m

    type (p(4)) :: p1 ! tcx: (4)

    allocate (p1%b(2))

end



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: p - added parameters (kp_1) to invoke with (4) / declare with (4) - 1 changes