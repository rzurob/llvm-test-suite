! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal514a14k
!*
!*  DATE                       : 2007-11-07 (original: 04/19/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of the temps created by
!*                               function result in ASSOCIATE construct)
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
        integer(kbase_1) :: id = 1

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'
        b%id = -1
    end subroutine
end module

program ffinal514a14k
use m
    interface
        type(base(4)) function makeData (i) ! tcx: (4)
        use m
            integer*4, intent(in) :: i
        end function
    end interface

    associate (x => makeData(10))
        print *, x
    end associate

    print *, 'end'
end

type(base(4)) function makeData (i) ! tcx: (4)
use m
    integer*4, intent(in) :: i

    makeData%id = i
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
