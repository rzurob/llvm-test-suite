! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal514a0k
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal514a0 by Jim Xia)
!*  DATE                       : 2007-10-12 (original: 04/12/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : final sub (temporary array objects produced by
!*                               function need to be finalized after the use in
!*                               the intrinsic assignment)
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

        final :: finalizeBase, finalizeBaseRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'

        b%id = 0
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(inout) :: b(:) ! tcx: (4)

        print *, 'finalizeBaseRank1'

        b%id = 0
    end subroutine

    function produceBaseArray (b, n)
        type (base(4)), intent(in) :: b ! tcx: (4)
        integer, intent(in) :: n
        type (base(4)) :: produceBaseArray(n) ! tcx: (4)

        produceBaseArray%id = b%id
    end function

    type (base(4)) function produceBase (b) ! tcx: (4)
        type (base(4)), intent(in) :: b ! tcx: (4)

        produceBase%id = b%id
    end function
end module

program ffinal514a0k
use m
    type (base(4)), save :: b1 (5), b2 ! tcx: (4)

    b2%id = 10

    b1 = produceBaseArray (b2, 5)

    b2 = produceBase (b2)

    print *, 'end'

    if (b2%id /= 10) error stop 101_4
    if (any (b1%id /= 10)) error stop 2_4
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 7 changes
