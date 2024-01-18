! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal513a0k
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal513a0 by Jim Xia)
!*  DATE                       : 2007-10-16 (original: 04/14/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf90)
!*
!*  DESCRIPTION                : final sub (very basic test for intent(out)
!*                               finalization)
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

    type (base(4)) :: b1_m(2) ! tcx: (4)

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal513a0k
use m
    call abc (b1_m(1))

    call cba (b1_m(2))

    print *, b1_m
    contains

    subroutine abc (b)
        type (base(4)), intent(out) :: b ! tcx: (4)

        print *, 'abc'

        b%id = 1
    end subroutine

    subroutine cba (b)
        class (base(4)), intent(out) :: b ! tcx: (4)

        print *, 'cba'

        b%id = 2
    end subroutine
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
