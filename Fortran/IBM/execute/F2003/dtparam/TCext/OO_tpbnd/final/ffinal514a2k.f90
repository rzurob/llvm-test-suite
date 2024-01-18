! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-12 (original: 04/12/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of temporaries created
!*                               by function calls in an array constructor)
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

        procedure :: replicate => produceBase

        final :: finalizeBase
    end type

    type (base(4)) :: b1_m(3), b2_m(3) ! tcx: (4)

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'

        b%id = 0
    end subroutine

    type (base(4)) function produceBase (b) ! tcx: (4)
        class (base(4)), intent(in) :: b ! tcx: (4)

        produceBase%id = b%id
    end function
end module

program ffinal514a2k
use m
    b2_m%id = (/1, 2, 3/)

    b1_m = (/(b2_m(i)%replicate(), i=1,3)/)

    print *, 'end'

    if (any (b1_m%id /= (/1, 2, 3/))) error stop 101_4
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
