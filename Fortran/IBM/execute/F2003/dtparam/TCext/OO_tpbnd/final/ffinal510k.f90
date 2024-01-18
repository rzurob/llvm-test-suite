! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal510k
!*
!*  DATE                       : 2007-10-12 (original: 04/05/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (local variables introduced by
!*                               IMPLICIT statement in a subprogram)
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
        integer(kbase_1), pointer :: data => null()

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'
        if (associated (b%data)) deallocate (b%data)
    end subroutine
end module

program ffinal510k
use m
    call abc

    print *, 'end of program'
end

subroutine abc
use m
    implicit type (base(4)) (b) ! tcx: (4)

    target b2

    parameter (b_const = base(4) (null())) ! tcx: (4)

    print *, 'end of abc'
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
