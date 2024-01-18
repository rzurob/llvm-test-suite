! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal005a3kkk
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal005a3 by Jim Xia)
!*  DATE                       : 2007-10-31 (original: 02/08/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
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

    type container (kcontainer_1,kcontainer_2) ! kcontainer_1,kcontainer_2=4,8
       integer, kind :: kcontainer_1,kcontainer_2
        class (b(kcontainer_2)), allocatable :: b2    !<-- b2 finalized when b2 is deallocated ! tcx: (kcontainer_2)
        type (A(kcontainer_1)) a1 ! tcx: (kcontainer_1)
        type (B(kcontainer_2)) b1 ! tcx: (kcontainer_2)
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

program ffinal005a3kkk
use m
    class (container(4,8)), pointer :: co1(:) ! tcx: (4,8)

    allocate (co1(2))
    allocate (co1(1)%b2, co1(1)%b1%r, co1(2)%b2, co1(2)%b1%r)
    allocate (co1(1)%b2%r)

    deallocate (co1)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: A - added parameters (kA) to invoke with (4) / declare with (4) - 2 changes
! type: B - added parameters (kB_1) to invoke with (8) / declare with (8) - 3 changes
! type: container - added parameters (kcontainer_1,kcontainer_2) to invoke with (4,8) / declare with (4,8) - 1 changes
