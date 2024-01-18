! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal500a1k
!*
!*  DATE                       : 2007-10-11 (original: 04/01/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                :
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

    subroutine finalizeBase(b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal500a1k
use m
    call abc

    print *, 'end'
end

subroutine abc
use m
    type (base(4)) :: b1 = base(4) (10) ! tcx: (4) ! tcx: (4)
    type (base(4)) :: b2 ! tcx: (4)

    print *, 'in abc'
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
