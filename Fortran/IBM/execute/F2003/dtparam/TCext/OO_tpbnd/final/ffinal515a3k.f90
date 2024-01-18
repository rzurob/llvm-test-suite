! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-11 (original: 04/20/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of the temps created by
!*                               structure constructor in a DO construct, in
!*                               particular DO WHILE loop)
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

    type (base(4)) :: b1_m(3) ! tcx: (4)

    contains

    subroutine finalizeBase (b)
        type (base(4)) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal515a3k
use m
    interface operator (>)
        logical function b1GTb2 (b1, b2)
        use m
            type (base(4)), intent(in) :: b1, b2 ! tcx: (4)
        end function
    end interface

    b1_m%id = 1

    i = 1

    do while ((i <= 3) .and. (b1_m(i) > base(4)(i-2))) ! tcx: (4)
        print *, i
        i = i + 1
    end do

    i = 1

    print *, 'send loop'

    do while ((i <= 3) .and. (b1_m(i) > base(4) (0))) ! tcx: (4)
        print *, i
        b1_m(i) = base(4)(i) ! tcx: (4)

        i = i + 1
    end do

    print *, 'end'
end

logical function b1GTb2 (b1, b2)
use m
    type (base(4)), intent(in) :: b1, b2 ! tcx: (4)

    b1GTb2 = (b1%id > b2%id)
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 7 changes
