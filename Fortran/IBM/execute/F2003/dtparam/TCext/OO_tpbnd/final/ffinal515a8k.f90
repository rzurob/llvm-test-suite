! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal515a8k
!*
!*  DATE                       : 2007-11-11 (original: 04/26/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (temps created by structure
!*                               constructor finalized; DO-construct)
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
        procedure, pass(b1) :: diff => IDdiff
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    integer*4 function IDdiff (b1, b2)
        type (base(4)), intent(in) :: b2 ! tcx: (4)
        class (base(4)), intent(in) :: b1 ! tcx: (4)

        IDdiff = (b1%id - b2%id)
    end function
end module

program ffinal515a8k
use m
    type (base(4)) :: b1 = base(4)(10) ! tcx: (4) ! tcx: (4)

    do i = 1, b1%diff(base(4)(5)) ! tcx: (4)
        print *, i
    end do

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 6 changes
