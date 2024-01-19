! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-12 (original: 04/12/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization for temporary scalar
!*                               used for assignment for array)
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
        integer(kbase_1) id

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(in) ::b(:) ! tcx: (4)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

program ffinal514a3k
use m
    interface
        type (base(4)) function createBase(i) ! tcx: (4)
            use m
            integer*4, intent(in) :: i
        end function
    end interface

    type (base(4)), save :: b1(10) ! tcx: (4)

    b1 = createBase (10)

    if (any (b1%id /= 10)) error stop 101_4
end

type (base(4)) function createBase(i) ! tcx: (4)
use m
    integer*4, intent(in) :: i

    createBase%id = i
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
