! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-31 (original: 02/08/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final subroutine (finalization of finalizable
!                               scalar components for an array)
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
    type A (kA) ! kA=4
       integer, kind :: kA
        contains

        final :: finalizeA
    end type

    type B (kB_1) ! kB_1=8
       integer, kind :: kB_1
        real(kB_1), allocatable :: r

        contains

        final :: finalizeB
    end type

    type container
        class (b(8)), allocatable :: b2    !<-- b2 finalized when b2 is deallocated ! tcx: (8)
        type (A(4)) a1 ! tcx: (4)
        type (B(8)) b1 ! tcx: (8)
    end type

    contains

    subroutine finalizeA (a1)
        type (A(4)), intent(in) :: a1 ! tcx: (4)

        print *, 'finalizeA'
    end subroutine

    subroutine finalizeB (b1)
        type (B(8)), intent(in) :: b1 ! tcx: (8)

        print *, 'finalizeB'
    end subroutine
end module

program ffinal005a3kk2
use m
    class (container), pointer :: co1(:)

    allocate (co1(2))
    allocate (co1(1)%b2, co1(1)%b1%r, co1(2)%b2, co1(2)%b1%r)
    allocate (co1(1)%b2%r)

    deallocate (co1)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: A - added parameters (kA) to invoke with (4) / declare with (4) - 2 changes
! type: B - added parameters (kB_1) to invoke with (8) / declare with (8) - 3 changes