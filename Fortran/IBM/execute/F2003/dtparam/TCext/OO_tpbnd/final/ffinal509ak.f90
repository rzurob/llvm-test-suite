! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal509ak
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal509a by Jim Xia)
!*  DATE                       : 2007-11-02 (original: 04/03/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf90)
!*
!*  DESCRIPTION                : final sub (finalization happens before default
!*                               initialization)
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
        integer(kbase_1) :: id = 10

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent (inout) :: b ! tcx: (4)

        print *, 'reset id to 0'

        b%id = 0
    end subroutine
end module

program ffinal509ak
use m
    interface
        subroutine sub (b)
        use m
            class (base(4)), intent(out) :: b ! tcx: (4)
        end subroutine
    end interface

    type (base(4)) :: b1 ! tcx: (4)

    b1%id = -1

    print *, 'call sub'

    call sub (b1)

    print *, 'after sub'

    if (b1%id /= 10) error stop 101_4
end

subroutine sub (b)
use m
    class (base(4)), intent (out) :: b ! tcx: (4)

    if (b%id /= 10) error stop 10_4
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
