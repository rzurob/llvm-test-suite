! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-12 (original: 04/14/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (function return result finalized;
!*                               use array)
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
        integer(kbase_1) :: id = 0

        contains

        final :: finalizeBase
        final :: finalizeBaseRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        implicit type(base(4)) (b) ! tcx: (4)
        dimension b(:)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

program ffinal513a2k
use m
    interface
        function makeBaseArray (id, n)
        use m
            integer*4, intent(in) :: id, n
            type (base(4)) :: makeBaseArray (n) ! tcx: (4)
        end function
    end interface

    print *, makeBaseArray (10, 2)
end

function makeBaseArray (id, n)
use m
    integer*4, intent(in) :: id, n
    type (base(4)) :: makeBaseArray (n) ! tcx: (4)

    makeBaseArray%id = id
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
