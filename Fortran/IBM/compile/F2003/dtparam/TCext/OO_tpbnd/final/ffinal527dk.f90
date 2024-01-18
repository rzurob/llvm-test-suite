! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-11 (original: 09/14/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (C1273: impure final binding cannot
!                               appear in a pure procedure)
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

    type container (kcontainer_1) ! kcontainer_1=4
       integer, kind :: kcontainer_1
        class (base(kcontainer_1)), pointer :: data ! tcx: (kcontainer_1)

        contains

        final :: finalizeData
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    !! in pure procedure, all references to procedures, including finalization,
    !! should be pure
    pure subroutine finalizeData (d)
        type (container(4)), intent(inout) :: d ! tcx: (4)

        if (associated (d%data)) deallocate (d%data)  !<-- impure procedure call
    end subroutine
end module

program ffinal527dk
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: container - added parameters (kcontainer_1) to invoke with (4) / declare with (4) - 1 changes
