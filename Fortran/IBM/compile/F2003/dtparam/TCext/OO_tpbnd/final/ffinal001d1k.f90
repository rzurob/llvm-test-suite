! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal001d1k
!*
!*  DATE                       : 2007-10-31 (original: 08/24/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final subroutine (C1273: Any procedure
!                               referenced in a pure subprogram, including one
!                               referenced via a defined-operation, assignment,
!                               or finalization, shall be pure)
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

!! there are three references of finalization in this subroutine
pure subroutine invalid
use m
    type (base(4)) :: b1  !<-- illegal: get finalized when procedure completes ! tcx: (4)

    b1 = base(4) (10)  !<-- two calls to finalizeBase, which is impure ! tcx: (4)
end subroutine

program ffinal001d1k
    call invalid
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
