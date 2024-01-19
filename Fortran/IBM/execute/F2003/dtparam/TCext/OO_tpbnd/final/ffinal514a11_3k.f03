! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-02 (original: 06/16/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of function returned
!                               temps in WHERE statement/construct)
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

    interface operator (==)
        elemental logical function baseEqual (b1, b2)
        import base
            type (base(4)), intent(in) :: b1, b2 ! tcx: (4)
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module


program ffinal514a11_3k
use m
    type (base(4)), save :: b1(10) ! tcx: (4)

    b1%id = (/2,3,5,5,6,7,7,7,9,10/)

    where (b1 == makeData (7))
        b1%id = -b1%id
    end where

    print *, 'second where'

    where (b1 == makeData (5)) b1%id = 105

    if (any(b1%id /= (/2,3,105,105,6,-7,-7,-7,9,10/))) error stop 101_4

    print *, 'end'
end

function makeBase (i)
use m, only: base
    type(base(4)) :: makeBase ! tcx: (4)
    intent(in) i

    makeBase%id = i
end function

elemental logical function baseEqual (b1, b2)
use m, only:base
    type (base(4)), intent(in) :: b1, b2 ! tcx: (4)

    baseEqual = (b1%id == b2%id)
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 6 changes
