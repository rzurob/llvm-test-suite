! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal513a3k
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal513a3 by Jim Xia)
!*  DATE                       : 2007-10-16 (original: 04/14/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : final sub (finalization of the actual arg
!*                               associated with INTENT(OUT) dummy-arg)
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

program ffinal513a3k
use m
    interface
        subroutine abc (b, b1)
        use m
            type (base(4)), intent(out) :: b(:) ! tcx: (4)
            type (base(4)), intent(in) :: b1 ! tcx: (4)
        end subroutine
    end interface

    type (base(4)), save :: b1(2) ! tcx: (4)

    call abc (b1, base(4)(10)) ! tcx: (4)

    if ((b1(1)%id /= 10) .or. (b1(2)%id /= 10)) error stop 101_4
end

subroutine abc (b, b1)
use m
    type (base(4)), intent(out) :: b(:) ! tcx: (4)
    type (base(4)), intent(in) :: b1 ! tcx: (4)

    b%id = b1%id
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 8 changes
