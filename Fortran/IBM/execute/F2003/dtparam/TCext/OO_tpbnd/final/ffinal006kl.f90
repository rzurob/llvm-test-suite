! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal006kl
!*
!*  DATE                       : 2007-10-31 (original: 02/08/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final subroutine (finalization of rank-one
!                               array component)
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
        integer(kbase_1) :: id = 1

        contains

        final :: finalizeBase, finalizeBaseArray1
    end type

    type container (kcontainer_1,lcontainer_1) ! kcontainer_1,lcontainer_1=4,2
       integer, kind :: kcontainer_1
       integer, len :: lcontainer_1
        type (base(kcontainer_1)) :: data(lcontainer_1) ! tcx: (kcontainer_1)
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base(4)), intent(in) :: b(:) ! tcx: (4)

        print *, 'finalizeBaseArray1'
    end subroutine
end module

program ffinal006kl
use m
    class (container(4,:)), allocatable :: co1(:) ! tcx: (4,:)

    allocate (container(4,2)::co1(2)) ! tcx: container(4,2)

    deallocate (co1)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: container - added parameters (kcontainer_1,lcontainer_1) to invoke with (4,2) / declare with (4,*) - 1 changes
