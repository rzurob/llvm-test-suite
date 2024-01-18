! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-12 (original: 04/14/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization not to be done for
!*                               structure constructor used in DATA statement)
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
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module

module m1
use m, only : base
    type dataType (kdataType_1) ! kdataType_1=4
       integer, kind :: kdataType_1
        type (base(kdataType_1)) :: b1 ! tcx: (kdataType_1)
    end type

    type dataType2 (kdataType2_1) ! kdataType2_1=4
       integer, kind :: kdataType2_1
        type (dataType(kdataType2_1)) :: d1 ! tcx: (kdataType2_1)
    end type
end module

program ffinal518akkk
    call abc
    call abc
end

subroutine abc
use m1
    type (dataType2(4)) :: d1 ! tcx: (4)

    data d1%d1%b1 /base(4)(10)/ ! tcx: (4)
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: dataType - added parameters (kdataType_1) to invoke with (4) / declare with (4) - 1 changes
! type: dataType2 - added parameters (kdataType2_1) to invoke with (4) / declare with (4) - 1 changes
