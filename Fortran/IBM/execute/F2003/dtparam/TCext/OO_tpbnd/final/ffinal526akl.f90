! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-11 (original: 04/23/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (if the entity being finalized is an
!*                               array, each finalizable component of each
!*                               element of that entity is finalized separately)
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

    type dataType (kdataType_1) ! kdataType_1=4
       integer, kind :: kdataType_1
        type (base(kdataType_1)) :: b1 ! tcx: (kdataType_1)
    end type

    type moreData (kmoreData_1,lmoreData_1) ! kmoreData_1,lmoreData_1=4,3
       integer, kind :: kmoreData_1
       integer, len :: lmoreData_1
        type (dataType(kmoreData_1)) :: d1(lmoreData_1) ! tcx: (kmoreData_1)
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal526akl

    call abc

    print *, 'end'
end

subroutine abc
use m
    type (moreData(4,3)) :: m1(5) ! tcx: (4,3)
end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: dataType - added parameters (kdataType_1) to invoke with (4) / declare with (4) - 1 changes
! type: moreData - added parameters (kmoreData_1,lmoreData_1) to invoke with (4,3) / declare with (4,*) - 1 changes
