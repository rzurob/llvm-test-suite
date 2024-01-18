! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal505ak
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal505a by Jim Xia)
!*  DATE                       : 2007-11-01 (original: 04/22/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : final sub (allocated allocatables in the main
!*                               program not finalized)
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
        integer(kbase_1) :: id = -1

        contains

        final :: finalizeBase
        final :: finalizeBaseRank1
    end type

    contains

    subroutine finalizeBase (b)
        type(base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(in) :: b(:) ! tcx: (4)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

module m1
use m
    type dataType (kdataType_1) ! kdataType_1=4
       integer, kind :: kdataType_1
        type (base(kdataType_1)), allocatable :: data1 ! tcx: (kdataType_1)
        class (base(kdataType_1)), allocatable :: data2(:) ! tcx: (kdataType_1)
    end type
end module

program ffinal505ak
use m1
    type (dataType(4)) :: d1 ! tcx: (4)

    allocate (d1%data1, d1%data2(3))
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
! type: dataType - added parameters (kdataType_1) to invoke with (4) / declare with (4) - 1 changes
