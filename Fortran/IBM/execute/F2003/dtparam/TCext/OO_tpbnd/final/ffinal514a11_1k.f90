! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal514a11_1k
!*
!*  DATE                       : 2007-11-02 (original: 04/16/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of the temp created by
!*                               the function return result; in a print
!*                               statement again)
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

    interface makeData
        function makeBase (i)
        import base
            type(base(4)) :: makeBase ! tcx: (4)
            integer*4, intent(in) :: i
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal514a11_1k
use m
    print *, makeData(10)

    print *, 'end'
end

type(base(4)) function makeBase (i) ! tcx: (4)
use m, only: base
    integer*4, intent(in) :: i

    makeBase%id = i
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
