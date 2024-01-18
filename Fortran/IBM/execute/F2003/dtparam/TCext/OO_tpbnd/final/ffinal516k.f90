! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal516k
!*
!*  DATE                       : 2007-11-11 (original: 02/15/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of function results
!                               used in specification expression)
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
        integer(kbase_1) :: id = 1

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    interface
        pure type (base(4)) function func1 (i) ! tcx: (4)
        import base
            integer*4, intent(in) :: i
            dimension func1 (10:i+10)
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(in) :: b(:) ! tcx: (4)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine abc (func, i1)
        interface
            pure type (base(4)) function func (i) ! tcx: (4)
            import base
                integer*4, intent(in) :: i
                dimension func (10:i+10)
            end function
        end interface

        integer(4), intent(in) :: i1 (lbound(func(5), 1):ubound(func(10), 1))

        print *, 'start abc'

        print *, lbound(func(4)), ubound(func(10))
        print *, size(i1)
    end subroutine
end module

program ffinal516k
use m
    procedure(func1) produceArray

    integer(4) i11(11)

    call abc (produceArray, i11)
end

pure type (base(4)) function produceArray (i) ! tcx: (4)
use m, only: base
    integer*4, intent(in) :: i
    dimension produceArray (10:i+10)

    produceArray%id = 0
end function



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
