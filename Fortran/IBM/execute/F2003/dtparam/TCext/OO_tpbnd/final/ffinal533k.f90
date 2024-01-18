! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal533k
!*
!*  DATE                       : 2007-11-11 (original: 02/15/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (deallocation of allocated
!                               allocatable subobject during deallocate)
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

    type container (kcontainer_1) ! kcontainer_1=4
       integer, kind :: kcontainer_1
        class (base(kcontainer_1)), allocatable :: data ! tcx: (kcontainer_1)
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal533k
use m
    type (container(4)), pointer :: co1(:) ! tcx: (4)

    allocate (co1(2))
    allocate (co1(1)%data, co1(2)%data)

    deallocate (co1)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: container - added parameters (kcontainer_1) to invoke with (4) / declare with (4) - 1 changes
