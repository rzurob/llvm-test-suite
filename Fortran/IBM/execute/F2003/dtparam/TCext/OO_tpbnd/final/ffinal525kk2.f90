! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal525kk2
!*
!*  DATE                       : 2007-11-11 (original: 06/18/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization sequence; step 2 vs
!                               step 3)
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
    type dataType (kdataType_1) ! kdataType_1=4
       integer, kind :: kdataType_1
        logical(kdataType_1) :: flag

        contains

        final :: finalizeData
    end type

    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: id

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child (kchild_1) ! kchild_1=4
       integer, kind :: kchild_1
        type (dataType(kchild_1)) :: data1 ! tcx: (kchild_1)
        class (dataType(kbase_1)), allocatable :: data2 ! tcx: (kchild_1)
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeData (d)
        type (dataType(4)), intent(in) :: d ! tcx: (4)

        print *, 'finalizeDataType'
    end subroutine
end module

program ffinal525kk2
    print *, 'call abc'

    call abc

    print *, 'call cba'

    call cba

    print *, 'end'
end

subroutine abc
use m
    type (child(4,4)) :: c1 ! tcx: (4,4)
end subroutine

subroutine cba
use m
    type (child(4,4)) :: c1 ! tcx: (4,4)

    allocate (c1%data2)
end subroutine


! Extensions to introduce derived type parameters:
! type: dataType - added parameters (kdataType_1) to invoke with (4) / declare with (4) - 3 changes
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 1 changes
! type: child - added parameters (kchild_1) to invoke with (4,4) / declare with (4,4) - 2 changes
