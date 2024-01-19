! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-11 (original: 02/15/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of temp created by
!                               structure constructor in where construct; in
!                               mask-expr)
!*
!*  KEYWORD(S)                 :
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

    interface operator (>=)
        elemental logical function b1GEb2 (b1, b2)
        import base
            class (base(4)), intent(in) :: b1, b2 ! tcx: (4)
        end function
    end interface


    interface operator (<)
        elemental logical function b1LTb2 (b1, b2)
        import base
            class (base(4)), intent(in) :: b1, b2 ! tcx: (4)
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module


elemental logical function b1LTb2 (b1, b2)
use m, only : base
    class (base(4)), intent(in) :: b1, b2 ! tcx: (4)

    b1LTb2 = (b1%id < b2%id)
end function

elemental logical function b1GEb2 (b1, b2)
use m, only : base
    class (base(4)), intent(in) :: b1, b2 ! tcx: (4)

    b1GEb2 = (b1%id >= b2%id)
end function

program ffinal515a7k
use m
    type (base(4)) :: b1 (10) ! tcx: (4)

    b1%id = (/(i,i=1,10)/)

    where (b1 >= base(4) (6)) ! tcx: (4)
        b1%id = 5
    elsewhere ((b1 < base(4)(5)) .and. (b1 >= base(4)(2))) ! tcx: (4) ! tcx: (4)
        b1%id = 2
    end where

    print *, b1
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 9 changes
