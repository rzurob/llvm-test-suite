! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal523k
!*
!*  DATE                       : 2007-11-11 (original: 04/15/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of the temporaries in
!*                               final subroutine -- ouch)
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

    recursive subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        integer*4, save :: counts = 0

        if (counts < 5) then
            counts = counts + 1
            print *, 'finalizeBase', base(4)(10) ! tcx: (4)
        else
            return
        end if
    end subroutine
end module

program ffinal523k
use m
    call abc
end

subroutine abc
use m
    type (base(4)) :: b1 ! tcx: (4)
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
